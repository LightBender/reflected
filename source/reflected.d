module reflected;

public import std.meta : Unqual;

public immutable ModuleDefinition testModule = reflectModule!"reflected";
public immutable StructDefinition testStruct = reflectStruct!ModuleDefinition;
public immutable ClassDefinition testClass = reflectClass!ClassDefinition;
public immutable EnumDefinition testEnum = reflectEnum!Protection;

public enum Protection {
    Export,
    Public,
    Package,
    Protected,
    Private
}

public enum Primitive {
    void_,
    bool_,
    byte_,
    ubyte_,
    short_,
    ushort_,
    int_,
    uint_,
    long_,
    ulong_,
    cent_,
    ucent_,
    float_,
    double_,
    real_,
    ifloat_,
    idouble_,
    ireal_,
    cfloat_,
    cdouble_,
    creal_,
    char_,
    wchar_,
    dchar_,
    string_,
    wstring_,
    dstring_,
}

public immutable struct ModuleDefinition {
    public immutable string name;
    public immutable string fqn;

    public immutable FieldDefinition[] fields;
    public immutable EnumDefinition[] enumerations;
    public immutable FunctionDefinition[] functions;
    public immutable StructDefinition[] structs;
    public immutable InterfaceDefinition[] interfaces;
    public immutable ClassDefinition[] classes;

    private this(const string fqn, const string name, immutable(FieldDefinition)[] fields, immutable(EnumDefinition)[] enumerations, immutable(FunctionDefinition)[] functions, immutable(StructDefinition)[] structs, immutable(InterfaceDefinition)[] interfaces, immutable(ClassDefinition)[] classes) {
        this.fqn = fqn;
        this.name = name;

        this.fields = cast(immutable)fields;
        this.enumerations = cast(immutable)enumerations;
        this.functions = cast(immutable)functions;
        this.structs = cast(immutable)structs;
        this.interfaces = cast(immutable)interfaces;
        this.classes = cast(immutable)classes;
    }
}

public immutable struct FieldDefinition {
    public immutable string name;
    public immutable TypeDefinition type;

    public immutable Protection protection;
    public immutable bool isShared;
    public immutable bool isConst;
    public immutable bool isImmutable;

    public immutable size_t size;
    public immutable size_t offset;

    private immutable this(const string name, const TypeDefinition type, const Protection protection, const bool isShared, const bool isConst, const bool isImmutable, const size_t size, const size_t offset) {
        this.name = cast(immutable)name;
        this.type = cast(immutable)type;
        this.protection = cast(immutable)protection;
        this.isShared = cast(immutable)isShared;
        this.isConst = cast(immutable)isConst;
        this.isImmutable = cast(immutable)isImmutable;
        this.size = cast(immutable)size;
        this.offset = cast(immutable)offset;
    }
}

public immutable struct FunctionDefinition {
    public immutable string name;
    public immutable string fqn;

    public immutable ParameterDefinition[] parameters;

    public immutable TypeDefinition returnType;
    public immutable bool isRef;
    public immutable bool isShared;
    public immutable bool isConst;
    public immutable bool isImmutable;
    public immutable bool isInout;
    public immutable bool isAbstract;
    public immutable bool isFinal;

    public immutable Protection protection;
    public immutable bool isProperty;
    public immutable bool isNothrow;
    public immutable bool isPure;
    public immutable bool isNogc;
    public immutable bool isSafe;
    public immutable bool isTrusted;
    public immutable bool isSystem;

    private this(const string fqn, const string name, immutable(ParameterDefinition)[] parameters, const TypeDefinition returnType, const bool isRef, const bool isShared, const bool isConst, const bool isImmutable, const bool isInout, bool isAbstract, bool isFinal, const Protection protection, const bool isProperty, const bool isNothrow, const bool isPure, const bool isNogc, const bool isSafe, const bool isTrusted, const bool isSystem) {
        this.name = cast(immutable)name;
        this.fqn = cast(immutable)fqn;
        this.parameters = cast(immutable)parameters;
        this.isRef = cast(immutable)isRef;
        this.isShared = cast(immutable)isShared;
        this.isConst = cast(immutable)isConst;
        this.isImmutable = cast(immutable)isImmutable;
        this.isInout = cast(immutable)isInout;
        this.isAbstract = cast(immutable)isAbstract;
        this.isFinal = cast(immutable)isFinal;
        this.protection = cast(immutable)protection;
        this.isProperty = cast(immutable)isProperty;
        this.isNothrow = cast(immutable)isNothrow;
        this.isPure = cast(immutable)isPure;
        this.isNogc = cast(immutable)isNogc;
        this.isSafe = cast(immutable)isSafe;
        this.isTrusted = cast(immutable)isTrusted;
        this.isSystem = cast(immutable)isSystem;
    }
}

public immutable struct ParameterDefinition {
    public immutable string name;
    public immutable TypeDefinition type;

    public immutable bool isShared;
    public immutable bool isConst;
    public immutable bool isImmutable;

    public immutable bool isLazy;
    public immutable bool isIn;
    public immutable bool isOut;
    public immutable bool isRef;
    public immutable bool isInout;
    public immutable bool isScope;
    public immutable bool isReturn;

    private immutable this(string name, immutable(TypeDefinition) type, bool isShared, bool isConst, bool isImmutable, bool isLazy, bool isIn, bool isOut, bool isRef, bool isInout, bool isScope, bool isReturn) {
        this.name = cast(immutable)name;
        this.type = cast(immutable)type;

        this.isShared = cast(immutable)isShared;
        this.isConst = cast(immutable)isConst;
        this.isImmutable = cast(immutable)isImmutable;

        this.isLazy = cast(immutable)isImmutable;
        this.isIn = cast(immutable)isIn;
        this.isOut = cast(immutable)isOut;
        this.isRef = cast(immutable)isRef;
        this.isInout = cast(immutable)isInout;
        this.isScope = cast(immutable)isScope;
        this.isReturn = cast(immutable)isReturn;
    }
}

public immutable class TypeDefinition {
    public immutable string name;
    public immutable string fqn;
    private immutable TypeInfo _typeInfo;
    public @property auto typeInfo() { return _typeInfo; }

    public immutable bool isShared;
    public immutable bool isConst;
    public immutable bool isImmutable;

    private this(
        const string fqn,
        const string name,
        const TypeInfo typeInfo,
        const bool isShared,
        const bool isConst,
        const bool isImmutable)
    {
        this.fqn = fqn;
        this.name = name;
        this._typeInfo = cast(immutable)typeInfo;
        this.isShared = isShared;
        this.isConst = isConst;
        this.isImmutable = isImmutable;
    }
}

public immutable class PrimitiveDefinition : TypeDefinition {
    public immutable Primitive primitive;

    private this(const string name, const TypeInfo type, const string typeName) {
        super(name, name, typeInfo, false, false, false);
        if(typeName == "void") this.primitive = Primitive.void_;
        else if(typeName == "bool") this.primitive = Primitive.bool_;
        else if(typeName == "byte") this.primitive = Primitive.byte_;
        else if(typeName == "ubyte") this.primitive = Primitive.ubyte_;
        else if(typeName == "short") this.primitive = Primitive.short_;
        else if(typeName == "ushort") this.primitive = Primitive.ushort_;
        else if(typeName == "int") this.primitive = Primitive.int_;
        else if(typeName == "uint") this.primitive = Primitive.uint_;
        else if(typeName == "long") this.primitive = Primitive.long_;
        else if(typeName == "ulong") this.primitive = Primitive.ulong_;
        //else if(typeName == "cent") this.primitive = Primitive.cent_;
        //else if(typeName == "ucent") this.primitive = Primitive.ucent_;
        else if(typeName == "real") this.primitive = Primitive.real_;
        else if(typeName == "float") this.primitive = Primitive.float_;
        else if(typeName == "double") this.primitive = Primitive.double_;
        else if(typeName == "ireal") this.primitive = Primitive.ireal_;
        else if(typeName == "ifloat") this.primitive = Primitive.ifloat_;
        else if(typeName == "idouble") this.primitive = Primitive.idouble_;
        else if(typeName == "cfloat") this.primitive = Primitive.cfloat_;
        else if(typeName == "cdouble") this.primitive = Primitive.cdouble_;
        else if(typeName == "creal") this.primitive = Primitive.creal_;
        else if(typeName == "char") this.primitive = Primitive.char_;
        else if(typeName == "wchar") this.primitive = Primitive.wchar_;
        else if(typeName == "dchar") this.primitive = Primitive.dchar_;
        else if(typeName == "string") this.primitive = Primitive.string_;
        else if(typeName == "wstring") this.primitive = Primitive.wstring_;
        else if(typeName == "dstring") this.primitive = Primitive.dstring_;
    }
}

public immutable class ArrayDefinition : TypeDefinition {
    public immutable TypeDefinition definition;
    public immutable int dimensions;

    private this(const string fqn, const TypeInfo typeInfo, const TypeDefinition definition, const int dimensions) {
        super(fqn, fqn, cast(TypeInfo)typeInfo, false, false, false);
        this.definition = cast(immutable)definition;
        this.dimensions = dimensions;
    }
}

public immutable class AssociativeArrayDefinition : TypeDefinition {
    public immutable TypeDefinition keyDefinition;
    public immutable TypeDefinition valueDefinition;

    private this(const string fqn, const TypeInfo typeInfo, const TypeDefinition keyDefinition, const TypeDefinition valueDefinition) {
        super(fqn, fqn, cast(TypeInfo)typeInfo, false, false, false);
        this.keyDefinition = cast(immutable)keyDefinition;
        this.valueDefinition = cast(immutable)valueDefinition;
    }
}

public immutable class EnumDefinition : TypeDefinition {
    public immutable TypeInfo baseType;
    public immutable Protection protection;
    public immutable EnumValue[] values;

    private this(const string fqn, const string name, const TypeInfo_Enum typeInfo, const TypeInfo baseType, const Protection protection, immutable(EnumValue)[] values) {
        super(fqn, name, cast(TypeInfo)typeInfo, false, false, true);
        this.baseType = cast(immutable)baseType;
        this.protection = protection;
        this.values = cast(immutable)values;
    }
}

public immutable struct EnumValue {
    public immutable string name;
    public immutable string value;

    private this(const string name, const string value) {
        this.name = name;
        this.value = value;
    }
}

public final immutable class StructDefinition : TypeDefinition {
    public immutable Protection protection;

    public immutable FieldDefinition[] fields;
    public immutable FunctionDefinition[] methods;

    private immutable this(
        const string fqn,
        const string name,
        const TypeInfo_Struct typeInfo,
        const Protection protection,
        const bool isShared,
        const bool isConst,
        const bool isImmutable, 
        immutable(FieldDefinition)[] fields,
        immutable(FunctionDefinition)[] methods)
    {
        super(fqn, name, cast(TypeInfo)typeInfo, isShared, isConst, isImmutable);
        this.protection = cast(immutable)protection;
        this.fields = cast(immutable)fields;
        this.methods = cast(immutable)methods;
    }
}

public final immutable class InterfaceDefinition : TypeDefinition {
    public immutable Protection protection;
    public immutable FunctionDefinition[] methods;

    private immutable this(
        const string fqn,
        const string name,
        const TypeInfo_Interface typeInfo,
        const Protection protection,
        const bool isShared,
        const bool isConst,
        const bool isImmutable, 
        immutable(FunctionDefinition)[] methods)
    {
        super(fqn, name, cast(TypeInfo)typeInfo, isShared, isConst, isImmutable);
        this.protection = cast(immutable)protection;
        this.methods = cast(immutable)methods;
    }
}

public final immutable class ClassDefinition : TypeDefinition {
    public immutable Protection protection;
    public immutable bool isAbstract;
    public immutable bool isFinal;

    public immutable FieldDefinition[] fields;
    public immutable FunctionDefinition[] methods;

    private immutable this(
        const string fqn,
        const string name,
        const TypeInfo_Class typeInfo,
        const Protection protection,
        const bool isShared,
        const bool isConst,
        const bool isImmutable,
        const bool isAbstract,
        const bool isFinal,
        immutable(FieldDefinition)[] fields,
        immutable(FunctionDefinition)[] methods)
    {
        super(fqn, name, cast(TypeInfo)typeInfo, isShared, isConst, isImmutable);
        this.protection = cast(immutable)protection;
        this.isAbstract = cast(immutable)isAbstract;
        this.isFinal = cast(immutable)isFinal;
        this.fields = cast(immutable)fields;
        this.methods = cast(immutable)methods;
    }
}

public immutable(TypeDefinition) reflectType(T)() {
    import std.traits : fullyQualifiedName, isBasicType, isSomeString, isArray, isAssociativeArray;
    import std.range.primitives : ElementType;

    static if(is(Unqual!T == enum)) {
        return reflectEnum!T;
    } else static if(is(Unqual!T == struct)) {
        return reflectStruct!T;
    } else static if(is(Unqual!T == class)) {
        return reflectClass!T;
    } else static if(is(Unqual!T == interface)) {
        return reflectInterface!T;
    } else static if(isBasicType!(Unqual!T) || isSomeString!T) {
        return new immutable PrimitiveDefinition(fullyQualifiedName!T, typeid(T), fullyQualifiedName!T);
    } else static if(isArray!(Unqual!T)) {
        return new immutable ArrayDefinition(fullyQualifiedName!T, typeid(T), reflectType!(ElementType!T), arrayDimensions!T);
    } else static if(isAssociativeArray!(Unqual!T)) {
        return new immutable AssociativeArrayDefinition(fullyQualifiedName!T, typeid(T), reflectType!(ElementType!(T.keys)), reflectType!(ElementType!(T.values)));
    }
}

public immutable(ModuleDefinition) reflectModule(string module_)() {
    import std.traits : fullyQualifiedName, isFunction, Fields, FieldNameTuple;

    mixin(`import dmodule = ` ~ module_ ~ `;`);
    alias members = __traits(allMembers, dmodule);
    const string fqn = fullyQualifiedName!dmodule;
    const string name = __traits(identifier, dmodule);

    immutable(FieldDefinition)[] fields;
    immutable(FunctionDefinition)[] functions;
    immutable(EnumDefinition)[] enumerations;
    immutable(StructDefinition)[] structs;
    immutable(ClassDefinition)[] classes;
    immutable(InterfaceDefinition)[] interfaces;

    static foreach(member; members) {
        pragma(msg, fullyQualifiedName!(__traits(getMember, dmodule, member)));
        static if(isFunction!(__traits(getMember, dmodule, member))) {
            pragma(msg, "Reflected function: " ~ module_ ~ "." ~ member);
            functions ~= reflectFunction!(__traits(getMember, dmodule, member));
        } else static if (is(Unqual!(__traits(getMember, dmodule, member)) == enum)) {
            pragma(msg, "Reflected enumeration: " ~ module_ ~ "." ~ member);
            enumerations ~= reflectEnum!(__traits(getMember, dmodule, member));
        } else static if (is(Unqual!(__traits(getMember, dmodule, member)) == struct) || is(Unqual!(__traits(getMember, dmodule, member)) == union)) {
            pragma(msg, "Reflected struct: " ~ module_ ~ "." ~ member);
            structs ~= reflectStruct!(__traits(getMember, dmodule, member));
        } else static if (is(Unqual!(__traits(getMember, dmodule, member)) == class)) {
            pragma(msg, "Reflected class: " ~ module_ ~ "." ~ member);
            classes ~= reflectClass!(__traits(getMember, dmodule, member));
        } else static if (is(Unqual!(__traits(getMember, dmodule, member)) == interface)) {
            pragma(msg, "Reflected interface: " ~ module_ ~ "." ~ member);
            interfaces ~= reflectInterface!(__traits(getMember, dmodule, member));
        } else static if (is(typeof(Unqual!member))) {
            pragma(msg, "Reflected module member: " ~ module_ ~ "." ~ member);
            fields ~= immutable FieldDefinition(
                __traits(identifier, __traits(getMember, dmodule, member)),
                reflectType!(__traits(getMember, dmodule, member)),
                getProtection(__traits(getProtection, __traits(getMember, dmodule, member))),
                is(__traits(getMember, dmodule, member) == shared),
                is(__traits(getMember, dmodule, member) == const),
                is(__traits(getMember, dmodule, member) == immutable),
                (__traits(getMember, dmodule, member)).sizeof, 0);
        } else {
            pragma(msg, "Cannot reflect on type: " ~ member);
        }
    }

    return immutable ModuleDefinition(fqn, name, fields, enumerations, functions, structs, interfaces, classes);
}

public immutable(FunctionDefinition) reflectFunction(alias T)() {
    import std.algorithm : among;
    import std.traits: fullyQualifiedName, ReturnType, isAbstractFunction, isFinalFunction, Parameters, ParameterIdentifierTuple, ParameterStorageClass, ParameterStorageClassTuple;

    const string name = __traits(identifier, T);
    const string fqn = fullyQualifiedName!T;
    const auto type = reflectType!(Unqual!(ReturnType!T));
    const Protection protection = getProtection(__traits(getProtection, T));
    const bool isShared = "shared".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isConst = "const".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isImmutable = "immutable".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isInout = "inout".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isPure = "pure".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isRef = "ref".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isNothrow = "nothrow".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isNogc = "@nogc".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isProperty = "@property".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isSafe = "@safe".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isTrusted = "@trusted".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isSystem = "@system".among(__traits(getFunctionAttributes, T)) != 0;
    const bool isAbstract = isAbstractFunction!T;
    const bool isFinal = isFinalFunction!T;

    alias paramTypes = Parameters!T;
    alias paramNames = ParameterIdentifierTuple!T;
    alias paramStorage = ParameterStorageClassTuple!T;

    immutable(ParameterDefinition)[] parameters;
    foreach(pc, pt; paramTypes) {
        const bool isParamShared = is(pt == shared);
        const bool isParamConst = is(pt == const);
        const bool isParamImmutable = is(pt == immutable);
        const bool isParamInout = is(pt == inout);
        const bool isParamLazy = paramStorage[pc] == ParameterStorageClass.lazy_;
        const bool isParamOut = paramStorage[pc] == ParameterStorageClass.out_;
        const bool isParamRef = paramStorage[pc] == ParameterStorageClass.ref_;
        const bool isParamScope = paramStorage[pc] == ParameterStorageClass.scope_;
        const bool isParamReturn = paramStorage[pc] == ParameterStorageClass.return_;
        parameters ~= immutable ParameterDefinition(paramNames[pc], reflectType!pt, isParamShared, isParamConst, isParamImmutable, isParamLazy, isParamConst, isParamOut, isParamRef, isParamInout, isParamScope, isParamReturn);
    }

    return immutable FunctionDefinition(fqn, name, parameters, type, isRef, isShared, isConst, isImmutable, isInout, isAbstract, isFinal, protection, isProperty, isNothrow, isPure, isNogc, isSafe, isTrusted, isSystem);
}

public immutable(EnumDefinition) reflectEnum(T)() if(is(Unqual!T == enum)) {
    import std.traits : fullyQualifiedName, OriginalType, EnumMembers;
    import std.conv : to;

    const string fqn = fullyQualifiedName!T;
    const string name = __traits(identifier, T);
    const TypeInfo_Enum type = typeid(Unqual!T);
    const TypeInfo baseType = typeid(OriginalType!T);
    const Protection protection = getProtection(__traits(getProtection, T));

    alias memberNames = EnumMembers!T;
    auto memberValues = cast(OriginalType!T[])[EnumMembers!T];
    immutable(EnumValue)[] values;
    foreach(ec, em; memberNames) {
        values ~= immutable EnumValue(__traits(identifier, em), to!string(memberValues[ec]));
    }

    return new immutable EnumDefinition(fqn, name, type, baseType, protection, values);
}

public immutable(StructDefinition) reflectStruct(T)() if(is(Unqual!T == struct)) {
    import std.traits : moduleName, fullyQualifiedName, isFunction, Fields, FieldNameTuple;
    import std.conv : to;
    mixin("import " ~ moduleName!T ~ ";");

    const string fqn = fullyQualifiedName!T;
    const string name = __traits(identifier, T);
    const TypeInfo_Struct type = typeid(Unqual!T);
    const Protection protection = getProtection(__traits(getProtection, T));
    const bool isShared = is(T == shared);
    const bool isConst = is(T == const);
    const bool isImmutable = is(T == immutable);

    immutable(FunctionDefinition)[] functions;
    alias members = __traits(allMembers, T);
    static foreach(m; members) {
        static if (isFunction!m || __traits(identifier, m) == "__ctor") {
            foreach(o; __traits(getOverloads, T, m)) {
                functions ~= reflectFunction!o;
            }
        }
    }

    immutable(FieldDefinition)[] fields;
    alias fieldTypes = Fields!T;
    alias fieldNames = FieldNameTuple!T;
    size_t offset = 0;
    foreach(fc, fn; fieldNames) {
        alias ft = fieldTypes[fc];
        mixin("const Protection fp = getProtection(__traits(getProtection, " ~ fullyQualifiedName!T ~ "." ~ fn ~ "));");
        //mixin("const size_t offset = " ~ fullyQualifiedName!T ~ "." ~ fn ~ ".offsetof;");
        fields ~= immutable FieldDefinition(fn, reflectType!ft, fp, is(ft == shared), is(ft == const), is(ft == immutable), ft.sizeof, offset);
        offset += ft.sizeof; //Workaround for https://issues.dlang.org/show_bug.cgi?id=15371
    }

    return new immutable StructDefinition(fqn, name, type, protection, isShared, isConst, isImmutable, fields, functions);
}

public immutable(ClassDefinition) reflectClass(T)() if(is(Unqual!T == class)) {
    import std.traits : moduleName, fullyQualifiedName, isAbstractClass, isFinalClass, isFunction, Fields, FieldNameTuple;
    import std.conv : to;
    mixin("import " ~ moduleName!T ~ ";");

    const string fqn = fullyQualifiedName!T;
    const string name = __traits(identifier, T);
    const TypeInfo_Class type = typeid(Unqual!T);
    const Protection protection = getProtection(__traits(getProtection, T));
    const bool isShared = is(T == shared);
    const bool isConst = is(T == const);
    const bool isImmutable = is(T == immutable);
    const bool isAbstract = isAbstractClass!T;
    const bool isFinal = isFinalClass!T;

    immutable(FunctionDefinition)[] functions;
    alias members = __traits(allMembers, T);
    static foreach(m; members) {
        static if (isFunction!m || __traits(identifier, m) == "__ctor") {
            foreach(o; __traits(getOverloads, T, m)) {
                functions ~= reflectFunction!o;
            }
        }
    }

    immutable(FieldDefinition)[] fields;
    alias fieldTypes = Fields!T;
    alias fieldNames = FieldNameTuple!T;
    size_t offset = 0;
    foreach(fc, fn; fieldNames) {
        alias ft = fieldTypes[fc];
        mixin("const Protection fp = getProtection(__traits(getProtection, " ~ fullyQualifiedName!T ~ "." ~ fn ~ "));");
        //mixin("const size_t offset = " ~ fullyQualifiedName!T ~ "." ~ fn ~ ".offsetof;");
        fields ~= immutable FieldDefinition(fn, reflectType!ft, fp, is(ft == shared), is(ft == const), is(ft == immutable), ft.sizeof, offset);
        offset += ft.sizeof; //Workaround for https://issues.dlang.org/show_bug.cgi?id=15371
    }

    return new immutable ClassDefinition(fqn, name, type, protection, isShared, isConst, isImmutable, isAbstract, isFinal, fields, functions);
}

public immutable(InterfaceDefinition) reflectInterface(T)() if(is(Unqual!T == interface)) {
    import std.traits : fullyQualifiedName, isFunction, Fields, FieldNameTuple;
    import std.conv : to;

    const string fqn = fullyQualifiedName!T;
    const string name = __traits(identifier, T);
    const TypeInfo_Class type = typeid(Unqual!T);
    const Protection protection = getProtection(__traits(getProtection, T));
    const bool isShared = is(T == shared);
    const bool isConst = is(T == const);
    const bool isImmutable = is(T == immutable);

    FunctionDefinition[] functions;
    alias members = __traits(allMembers, T);
    foreach(m; members) {
        if (isFunction!m || __traits(identifier, m) == "__ctor") {
            foreach(o; __traits(getOverloads, T, m)) {
                functions ~= reflectFunction!o;
            }
        }
    }

    return new immutable InterfaceDefinition(fqn, name, type, protection, isShared, isConst, isImmutable, functions);
}

private Protection getProtection(string prot) {
    if (prot == "export") return Protection.Export;
    if (prot == "public") return Protection.Public;
    if (prot == "package") return Protection.Package;
    if (prot == "protected") return Protection.Protected;
    return Protection.Private;
}

private template arrayDimensions(T)
{
    static if(is(typeof(T.init[0])))
    {
        alias ElementType = typeof(T.init[0]);
        enum arrayDimensions = 1 + arrayDimensions!(ElementType);
    }
    else
    {
        enum arrayDimensions = 0;
    }
}