/*
 * Copyright 2010-2017 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jetbrains.kotlin.codegen.inline

import org.jetbrains.kotlin.backend.common.CodegenUtil
import org.jetbrains.kotlin.codegen.*
import org.jetbrains.kotlin.codegen.context.*
import org.jetbrains.kotlin.codegen.state.GenerationState
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.resolve.DescriptorToSourceUtils
import org.jetbrains.kotlin.resolve.calls.callUtil.getResolvedCallWithAssert
import org.jetbrains.kotlin.resolve.jvm.jvmSignature.JvmMethodSignature
import org.jetbrains.org.objectweb.asm.MethodVisitor
import org.jetbrains.org.objectweb.asm.Opcodes
import org.jetbrains.org.objectweb.asm.commons.Method
import org.jetbrains.org.objectweb.asm.tree.MethodNode

class SourceCompilerForInline(private val codegen: ExpressionCodegen) {

    fun generateMethodBody(
            adapter: MethodVisitor,
            descriptor: FunctionDescriptor,
            context: MethodContext,
            expression: KtExpression,
            jvmMethodSignature: JvmMethodSignature,
            codegen: BaseExpressionCodegen,
            lambdaInfo: ExpressionLambda?,
            state: GenerationState
    ): SMAP {
        val codegen = codegen as ExpressionCodegen
        val isLambda = lambdaInfo != null

        // Wrapping for preventing marking actual parent codegen as containing reified markers
        val parentCodegen = FakeMemberCodegen(
                codegen.parentCodegen, expression, context.parentContext as FieldOwnerContext<*>,
                if (isLambda)
                    codegen.parentCodegen.className
                else
                    state.typeMapper.mapImplementationOwner(descriptor).internalName
        )

        val strategy: FunctionGenerationStrategy
        if (expression is KtCallableReferenceExpression) {
            val callableReferenceExpression = expression
            val receiverExpression = callableReferenceExpression.receiverExpression
            val receiverType = if (receiverExpression != null && state.bindingContext.getType(receiverExpression) != null)
                state.typeMapper.mapType(state.bindingContext.getType(receiverExpression)!!)
            else
                null

            if (isLambda && lambdaInfo!!.isPropertyReference) {
                val asmType = state.typeMapper.mapClass(lambdaInfo.classDescriptor)
                val info = lambdaInfo.propertyReferenceInfo
                strategy = PropertyReferenceCodegen.PropertyReferenceGenerationStrategy(
                        true, info!!.getFunction, info.target, asmType, receiverType,
                        lambdaInfo.functionWithBodyOrCallableReference, state, true)
            }
            else {
                strategy = FunctionReferenceGenerationStrategy(
                        state,
                        descriptor,
                        callableReferenceExpression.callableReference
                                .getResolvedCallWithAssert(state.bindingContext),
                        receiverType, null,
                        true
                )
            }
        }
        else if (expression is KtFunctionLiteral) {
            strategy = ClosureGenerationStrategy(state, expression as KtDeclarationWithBody)
        }
        else {
            strategy = FunctionGenerationStrategy.FunctionDefault(state, expression as KtDeclarationWithBody)
        }

        FunctionCodegen.generateMethodBody(adapter, descriptor, context, jvmMethodSignature, strategy, parentCodegen)

        if (isLambda) {
            codegen.propagateChildReifiedTypeParametersUsages(parentCodegen.reifiedTypeParametersUsages)
        }

        return createSMAPWithDefaultMapping(expression, parentCodegen.orCreateSourceMapper.resultMappings)
    }

    private fun createSMAPWithDefaultMapping(
            declaration: KtExpression,
            mappings: List<FileMapping>
    ): SMAP {
        val containingFile = declaration.containingFile
        CodegenUtil.getLineNumberForElement(containingFile, true) ?: error("Couldn't extract line count in " + containingFile)

        return SMAP(mappings)
    }


    private class FakeMemberCodegen(
            internal val delegate: MemberCodegen<*>,
            declaration: KtElement,
            codegenContext: FieldOwnerContext<*>,
            private val className: String
    ) : MemberCodegen<KtPureElement>(delegate as MemberCodegen<KtPureElement>, declaration, codegenContext) {

        override fun generateDeclaration() {
            throw IllegalStateException()
        }

        override fun generateBody() {
            throw IllegalStateException()
        }

        override fun generateKotlinMetadataAnnotation() {
            throw IllegalStateException()
        }

        override fun getInlineNameGenerator(): NameGenerator {
            return delegate.inlineNameGenerator
        }

        override //TODO: obtain name from context
        fun getClassName(): String {
            return className
        }
    }

    fun doCreateMethodNodeFromSource(
            callableDescriptor: FunctionDescriptor,
            jvmSignature: JvmMethodSignature,
            codegen: BaseExpressionCodegen,
            context: CodegenContext<*>,
            callDefault: Boolean,
            state: GenerationState,
            asmMethod: Method
    ): SMAPAndMethodNode {
        val codegen = codegen as ExpressionCodegen
        val element = DescriptorToSourceUtils.descriptorToDeclaration(callableDescriptor)

        if (!(element is KtNamedFunction || element is KtPropertyAccessor)) {
            throw IllegalStateException("Couldn't find declaration for function " + callableDescriptor)
        }
        val inliningFunction = element as KtDeclarationWithBody?

        val node = MethodNode(
                API,
                AsmUtil.getMethodAsmFlags(callableDescriptor, context.contextKind, state) or if (callDefault) Opcodes.ACC_STATIC else 0,
                asmMethod.name,
                asmMethod.descriptor, null, null
        )

        //for maxLocals calculation
        val maxCalcAdapter = wrapWithMaxLocalCalc(node)
        val parentContext = context.parentContext ?: error("Context has no parent: " + context)
        val methodContext = parentContext.intoFunction(callableDescriptor)

        val smap: SMAP
        if (callDefault) {
            val implementationOwner = state.typeMapper.mapImplementationOwner(callableDescriptor)
            val parentCodegen = FakeMemberCodegen(
                    codegen.parentCodegen, inliningFunction!!, methodContext.parentContext as FieldOwnerContext<*>,
                    implementationOwner.internalName
            )
            if (element !is KtNamedFunction) {
                throw IllegalStateException("Property accessors with default parameters not supported " + callableDescriptor)
            }
            FunctionCodegen.generateDefaultImplBody(
                    methodContext, callableDescriptor, maxCalcAdapter, DefaultParameterValueLoader.DEFAULT,
                    inliningFunction as KtNamedFunction?, parentCodegen, asmMethod
            )
            smap = createSMAPWithDefaultMapping(inliningFunction, parentCodegen.orCreateSourceMapper.resultMappings)
        }
        else {
            smap = generateMethodBody(maxCalcAdapter, callableDescriptor, methodContext, inliningFunction!!, jvmSignature, codegen, null, state)
        }
        maxCalcAdapter.visitMaxs(-1, -1)
        maxCalcAdapter.visitEnd()

        return SMAPAndMethodNode(node, smap)
    }

    val inlineCallSiteInfo: InlineCallSiteInfo
        get() {
            var context = codegen.getContext()
            var parentCodegen = codegen.parentCodegen
            while (context is InlineLambdaContext) {
                val closureContext = context.getParentContext()
                assert(closureContext is ClosureContext) { "Parent context of inline lambda should be closure context" }
                assert(closureContext.parentContext is MethodContext) { "Closure context should appear in method context" }
                context = closureContext.parentContext as MethodContext
                assert(parentCodegen is FakeMemberCodegen) { "Parent codegen of inlined lambda should be FakeMemberCodegen" }
                parentCodegen = (parentCodegen as FakeMemberCodegen).delegate
            }

            val signature = codegen.state.typeMapper.mapSignatureSkipGeneric(context.functionDescriptor, context.contextKind)
            return InlineCallSiteInfo(
                    parentCodegen.className, signature.asmMethod.name, signature.asmMethod.descriptor
            )
        }

}