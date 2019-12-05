/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */
package org.jetbrains.kotlin.native.interop.gen

import kotlinx.metadata.*
import kotlinx.metadata.klib.*
import org.jetbrains.kotlin.utils.addIfNotNull

class StubIrMetadataEmitter(
        private val context: StubIrContext,
        private val builderResult: StubIrBuilderResult,
        private val moduleName: String
) {
    fun emit(): KlibModuleMetadata {
        val annotations = emptyList<KmAnnotation>()
        val fragments = emitModuleFragments()
        return KlibModuleMetadata(moduleName, fragments, annotations)
    }

    private fun emitModuleFragments(): List<KmModuleFragment> =
        ModuleMetadataEmitter(context.configuration.pkgName)(builderResult.stubs).let(::listOf)
}

/**
 * Translates single [StubContainer] to [KmModuleFragment].
 */
internal class ModuleMetadataEmitter(
        private val packageFqName: String
) {

    operator fun invoke(module: SimpleStubContainer): KmModuleFragment =
            VisitingContext().let {
                module.accept(visitor, it)
                writeModule(it)
            }

    private fun writeModule(visitingContext: VisitingContext) = KmModuleFragment().also { km ->
        km.fqName = packageFqName
        km.classes += visitingContext.classes.toList()
        km.pkg = writePackage(visitingContext)
    }

    private fun writePackage(visitingContext: VisitingContext) = KmPackage().also { km ->
        km.fqName = packageFqName
        km.typeAliases += visitingContext.typeAliases.toList()
        km.properties += visitingContext.properties.toList()
        km.functions += visitingContext.functions.toList()
    }

    private data class VisitingContext(
            val container: StubContainer? = null,
            val uniqIds: StubIrUniqIdProvider = StubIrUniqIdProvider(),
            val classes: MutableList<KmClass> = mutableListOf(),
            val properties: MutableList<KmProperty> = mutableListOf(),
            val typeAliases: MutableList<KmTypeAlias> = mutableListOf(),
            val functions: MutableList<KmFunction> = mutableListOf(),
            val constructors: MutableList<KmConstructor> = mutableListOf()
    )

    private val visitor = object : StubIrVisitor<VisitingContext, Unit> {
        override fun visitClass(element: ClassStub, data: VisitingContext) {
            if (element is ClassStub.Companion) {
                println("Skipping Companion")
                return
            }
            val classVisitingContext = data.copy(
                    container = element,
                    uniqIds = data.uniqIds.createChild("TODO"),
                    classes = data.classes
            )
            element.children.forEach { it.accept(this, classVisitingContext) }
            KmClass().apply {
                element.annotations.mapTo(annotations, AnnotationStub::map)
                flags = element.flags
                name = when (element) {
                    is ClassStub.Simple -> element.classifier.fqNameSerialized
                    is ClassStub.Enum -> element.classifier.fqNameSerialized
                    is ClassStub.Companion -> "Companion"
                }
                typeAliases += classVisitingContext.typeAliases.toList()
                properties += classVisitingContext.properties.toList()
                functions += classVisitingContext.functions.toList()
                constructors += classVisitingContext.constructors.toList()
                // TODO: Add names of nested classes
            }.let(data.classes::add)
        }

        override fun visitTypealias(element: TypealiasStub, data: VisitingContext) {
            KmTypeAlias(element.flags, element.alias.topLevelName).apply {
                uniqId = data.uniqIds.uniqIdForTypeAlias(element)
                underlyingType = element.aliasee.map(shouldExpandTypeAliases = false)
                expandedType = element.aliasee.map()
            }.let(data.typeAliases::add)
        }

        override fun visitFunction(element: FunctionStub, data: VisitingContext) {
            KmFunction(element.flags, element.name).apply {
                element.annotations.mapTo(annotations, AnnotationStub::map)
                returnType = element.returnType.map()
                element.parameters.mapTo(valueParameters, FunctionParameterStub::map)
                element.typeParameters.mapTo(typeParameters, TypeParameterStub::map)
                uniqId = data.uniqIds.uniqIdForFunction(element)
            }.let(data.functions::add)
        }

        override fun visitProperty(element: PropertyStub, data: VisitingContext) {
            KmProperty(element.flags, element.name, element.getterFlags, element.setterFlags).apply {
                element.annotations.mapTo(annotations, AnnotationStub::map)
                uniqId = data.uniqIds.uniqIdForProperty(element)
                returnType = element.type.map()
                if (element.kind is PropertyStub.Kind.Var) {
                    val setter = element.kind.setter
                    setter.annotations.mapTo(setterAnnotations, AnnotationStub::map)
                    // TODO: Maybe it's better to explicitly add setter parameter in stub.
                    setterParameter = FunctionParameterStub("value", element.type).map()
                }
                getterAnnotations += when (element.kind) {
                    is PropertyStub.Kind.Val -> element.kind.getter.annotations.map(AnnotationStub::map)
                    is PropertyStub.Kind.Var -> element.kind.getter.annotations.map(AnnotationStub::map)
                    is PropertyStub.Kind.Constant -> emptyList()
                }
                if (element.kind is PropertyStub.Kind.Constant) {
                    compileTimeValue = element.kind.constant.map()
                }
            }.let(data.properties::add)
        }

        override fun visitConstructor(constructorStub: ConstructorStub, data: VisitingContext) {
            KmConstructor(constructorStub.flags).apply {
                constructorStub.parameters.mapTo(valueParameters, FunctionParameterStub::map)
                constructorStub.annotations.mapTo(annotations, AnnotationStub::map)
            }.let(data.constructors::add)
        }

        override fun visitPropertyAccessor(propertyAccessor: PropertyAccessor, data: VisitingContext) {
            // TODO("not implemented")
        }

        override fun visitSimpleStubContainer(simpleStubContainer: SimpleStubContainer, data: VisitingContext) {
            simpleStubContainer.children.forEach { it.accept(this, data.copy(container = simpleStubContainer)) }
        }
    }
}

private fun flagsOfNotNull(vararg flags: Flag?): Flags =
        flagsOf(*listOfNotNull(*flags).toTypedArray())

private val VisibilityModifier.flags: Flags
    get() = flagsOfNotNull(
            Flag.IS_PUBLIC.takeIf { this == VisibilityModifier.PUBLIC },
            Flag.IS_PROTECTED.takeIf { this == VisibilityModifier.PROTECTED },
            Flag.IS_INTERNAL.takeIf { this == VisibilityModifier.INTERNAL },
            Flag.IS_PRIVATE.takeIf { this == VisibilityModifier.PRIVATE }
    )

private val FunctionStub.flags: Flags
    get() = flagsOfNotNull(
            Flag.Common.IS_PUBLIC,
            Flag.Function.IS_EXTERNAL,
            Flag.HAS_ANNOTATIONS.takeIf { annotations.isNotEmpty() },
            Flag.IS_FINAL.takeIf { modality == MemberStubModality.FINAL },
            Flag.IS_OPEN.takeIf { modality == MemberStubModality.OPEN },
            Flag.IS_ABSTRACT.takeIf { modality == MemberStubModality.ABSTRACT }
    )

private val Classifier.fqNameSerialized: String
    get() = buildString {
        if (pkg.isNotEmpty()) {
            append(pkg.replace('.', '/'))
            append('/')
        }
        // Nested classes should dot-separated.
        append(relativeFqName)
    }

private val PropertyStub.flags: Flags
    get() = flagsOfNotNull(
            Flag.IS_PUBLIC,
            Flag.Property.IS_DECLARATION,
            Flag.HAS_ANNOTATIONS.takeIf { annotations.isNotEmpty() },
            Flag.IS_FINAL.takeIf { modality == MemberStubModality.FINAL },
            when (kind) {
                is PropertyStub.Kind.Val -> null
                is PropertyStub.Kind.Var -> Flag.Property.IS_VAR
                is PropertyStub.Kind.Constant -> Flag.Property.IS_CONST
            },
            when (kind) {
                is PropertyStub.Kind.Constant -> null
                is PropertyStub.Kind.Val,
                is PropertyStub.Kind.Var -> Flag.Property.HAS_GETTER
            },
            when (kind) {
                is PropertyStub.Kind.Constant -> null
                is PropertyStub.Kind.Val -> null
                is PropertyStub.Kind.Var -> Flag.Property.HAS_SETTER
            }
    )

private val PropertyStub.getterFlags: Flags
    get() = when (kind) {
        is PropertyStub.Kind.Val -> kind.getter.flags
        is PropertyStub.Kind.Var -> kind.getter.flags
        is PropertyStub.Kind.Constant -> flagsOf()
    }

private val PropertyAccessor.Getter.flags: Flags
    get() = flagsOfNotNull(
            Flag.HAS_ANNOTATIONS.takeIf { annotations.isNotEmpty() },
            Flag.IS_PUBLIC,
            Flag.IS_FINAL,
            Flag.PropertyAccessor.IS_EXTERNAL.takeIf { this is PropertyAccessor.Getter.ExternalGetter }
    )

private val PropertyStub.setterFlags: Flags
    get() = if (kind !is PropertyStub.Kind.Var) flagsOf()
    else kind.setter.flags

private val PropertyAccessor.Setter.flags: Flags
    get() = flagsOfNotNull(
            Flag.HAS_ANNOTATIONS.takeIf { annotations.isNotEmpty() },
            Flag.IS_PUBLIC,
            Flag.IS_FINAL,
            Flag.PropertyAccessor.IS_EXTERNAL.takeIf { this is PropertyAccessor.Setter.ExternalSetter }
    )

private val StubType.flags: Flags
    get() = flagsOfNotNull(
            Flag.Type.IS_NULLABLE.takeIf { nullable }
    )

private val TypealiasStub.flags: Flags
    get() = flagsOfNotNull(
            Flag.IS_PUBLIC
    )

private val FunctionParameterStub.flags: Flags
    get() = flagsOfNotNull(
            Flag.HAS_ANNOTATIONS.takeIf { annotations.isNotEmpty() }
    )

private val ClassStub.flags: Flags
    get() = flagsOfNotNull(
            Flag.HAS_ANNOTATIONS.takeIf { annotations.isNotEmpty() },
            Flag.IS_PUBLIC,
            Flag.IS_OPEN.takeIf { this is ClassStub.Simple && this.modality == ClassStubModality.OPEN },
            Flag.Class.IS_COMPANION_OBJECT.takeIf { this is ClassStub.Companion },
            Flag.Class.IS_CLASS.takeIf { this is ClassStub.Simple },
            Flag.Class.IS_ENUM_CLASS.takeIf { this is ClassStub.Enum }
    )

private val ConstructorStub.flags: Flags
    get() = flagsOfNotNull(
            Flag.Constructor.IS_PRIMARY.takeIf { isPrimary },
            Flag.HAS_ANNOTATIONS.takeIf { annotations.isNotEmpty() }
    ) or visibility.flags

private fun String.mapToAnnotationArgument() =
        KmAnnotationArgument.StringValue(this)

private fun AnnotationStub.map(): KmAnnotation {
    val args = when (this) {
        AnnotationStub.ObjC.ConsumesReceiver -> TODO()
        AnnotationStub.ObjC.ReturnsRetained -> TODO()
        is AnnotationStub.ObjC.Method -> TODO()
        is AnnotationStub.ObjC.Factory -> TODO()
        AnnotationStub.ObjC.Consumed -> TODO()
        is AnnotationStub.ObjC.Constructor -> TODO()
        is AnnotationStub.ObjC.ExternalClass -> listOfNotNull(
                ("protocolGetter" to protocolGetter.mapToAnnotationArgument()).takeIf { protocolGetter.isNotEmpty() },
                ("binaryName" to binaryName.mapToAnnotationArgument()).takeIf { binaryName.isNotEmpty() }
        ).toMap()
        AnnotationStub.CCall.CString -> mapOf()
        AnnotationStub.CCall.WCString -> mapOf()
        is AnnotationStub.CCall.Symbol ->
            mapOf("id" to symbolName.mapToAnnotationArgument())
        is AnnotationStub.CStruct -> TODO()
        is AnnotationStub.CNaturalStruct -> TODO()
        is AnnotationStub.CLength -> TODO()
        is AnnotationStub.Deprecated -> TODO()
    }
    return KmAnnotation(classifier.fqNameSerialized, args)
}

/**
 * @param shouldExpandTypeAliases describes how should we write type aliases.
 * If [shouldExpandTypeAliases] is true then type alias-based types are written as
 * ```
 * Type {
 *  abbreviatedType = AbbreviatedType.abbreviatedClassifier
 *  classifier = AbbreviatedType.underlyingType
 *  arguments = AbbreviatedType.underlyingType.typeArguments
 * }
 * ```
 * So we basically replacing type alias with underlying class.
 * Otherwise:
 * ```
 * Type {
 *  classifier = AbbreviatedType.abbreviatedClassifier
 * }
 * ```
 * As of 25 Nov 2019, the latter form is used only for KmTypeAlias.underlyingType.
 */
// TODO: Add caching if needed.
private fun StubType.map(shouldExpandTypeAliases: Boolean = true): KmType = when (this) {
    is AbbreviatedType -> {
        val typeAliasClassifier = KmClassifier.TypeAlias(abbreviatedClassifier.fqNameSerialized)
        if (shouldExpandTypeAliases) {
            // Abbreviated and expanded types have the same nullability.
            KmType(flags).also { km ->
                km.abbreviatedType = KmType(flags).also { it.classifier = typeAliasClassifier }
                val kmUnderlyingType = underlyingType.map(true)
                km.arguments += kmUnderlyingType.arguments
                km.classifier = kmUnderlyingType.classifier
            }
        } else {
            KmType(flags).also { km -> km.classifier = typeAliasClassifier }
        }
    }
    is ClassifierStubType -> KmType(flags).also { km ->
        typeArguments.mapTo(km.arguments) { it.map(shouldExpandTypeAliases) }
        km.classifier = KmClassifier.Class(classifier.fqNameSerialized)
    }
    is FunctionalType -> KmType(flags).also { km ->
        typeArguments.mapTo(km.arguments) { it.map(shouldExpandTypeAliases) }
        km.classifier = KmClassifier.Class(classifier.fqNameSerialized)
    }
    is TypeParameterType -> KmType(flags).also { km ->
        km.classifier = KmClassifier.TypeParameter(id)
    }
}

private fun FunctionParameterStub.map(): KmValueParameter =
        KmValueParameter(flags, name).also { km ->
            val kmType = type.map()
            if (isVararg) {
                km.varargElementType = kmType
            } else {
                km.type = kmType
            }
            annotations.mapTo(km.annotations, AnnotationStub::map)
        }

private fun TypeParameterStub.map(): KmTypeParameter =
        KmTypeParameter(flagsOf(), name, id, KmVariance.INVARIANT).also { km ->
            km.upperBounds.addIfNotNull(upperBound?.map())
        }

private fun TypeArgument.map(expanded: Boolean=true): KmTypeProjection = when (this) {
    TypeArgument.StarProjection -> KmTypeProjection.STAR
    is TypeArgumentStub -> KmTypeProjection(variance.map(), type.map(expanded))
    else -> error("Unexpected TypeArgument: $this")
}

private fun TypeArgument.Variance.map(): KmVariance = when (this) {
    TypeArgument.Variance.INVARIANT -> KmVariance.INVARIANT
    TypeArgument.Variance.IN -> KmVariance.IN
    TypeArgument.Variance.OUT -> KmVariance.OUT
}

// TODO: Rename to mapToAnnotationArgument.
private fun ConstantStub.map(): KmAnnotationArgument<*> = when (this) {
    is StringConstantStub -> KmAnnotationArgument.StringValue(value)
    is IntegralConstantStub -> when (size) {
        1 -> if (isSigned) {
            KmAnnotationArgument.ByteValue(value.toByte())
        } else {
            KmAnnotationArgument.UByteValue(value.toByte())
        }
        2 -> if (isSigned) {
            KmAnnotationArgument.ShortValue(value.toShort())
        } else {
            KmAnnotationArgument.UShortValue(value.toShort())
        }
        4 -> if (isSigned) {
            KmAnnotationArgument.IntValue(value.toInt())
        } else {
            KmAnnotationArgument.UIntValue(value.toInt())
        }
        8 -> if (isSigned) {
            KmAnnotationArgument.LongValue(value)
        } else {
            KmAnnotationArgument.ULongValue(value)
        }

        else -> error("Integral constant of value $value with unexpected size of $size.")
    }
    is DoubleConstantStub -> when (size) {
        4 -> KmAnnotationArgument.FloatValue(value.toFloat())
        8 -> KmAnnotationArgument.DoubleValue(value)
        else -> error("Floating-point constant of value $value with unexpected size of $size.")
    }
}

private val TypeParameterType.id: Int
    get() = TODO()

private val TypeParameterStub.id: Int
    get() = TODO()