module tests.unittests;

import std.stdio;
import std.conv;

import reflected;

public int testField = 0;

//Test Module Reflection
public uint testModuleField = 0;
unittest {
    immutable ModuleDefinition reflected = reflectModule!"tests.unittests";
    assert(reflected.functions.length == 1);
    assert(reflected.enumerations.length == 1);
    assert(reflected.structs.length == 1);
    assert(reflected.classes.length == 1);
    assert(reflected.interfaces.length == 1);
    //assert(reflected.fields.length == 1);
}

public enum TestEnum {
    Value1 = 1,
    Value2 = 2,
    Value100 = 100,
}

//Test Module Functions
public int testFunction() { return 0; }
unittest {
    immutable FunctionDefinition reflected = reflectFunction!(__traits(getMember, tests.unittests, "testFunction"));
    assert(reflected.returnType.name == "int");
    assert(reflected.protection == Protection.Public);
    assert(reflected.parameters.length == 0);
}

//Test Enums
unittest {
    immutable EnumDefinition reflected = reflectEnum!TestEnum;
    assert(reflected.baseType == typeid(int));
    assert(reflected.protection == Protection.Public);
    assert(reflected.isShared == false);
    assert(reflected.isConst == false);
    assert(reflected.isImmutable == true);
    assert(reflected.values.length == 3);
}

class TestClass {
    export int exportField;
    public int publicField;
    public int[] arrayField;
    public int[string] aaField;
    package int packageField;
    protected int protectedField;
    private int privateField;

    public this(int field) {
        this.publicField = field;
    }

    public void testFunc1() { }
    public bool testFunc2() { return true; }
    public int testFunc3(int input) { return input; }

    private void privateMethod() {}
}

//Test Class
unittest {
    immutable ClassDefinition reflected = reflectClass!TestClass;
    assert(reflected.methods.length == 9);
    assert(reflected.fields.length == 7);
    version(X86_64) assert(reflected.alignment == 8);
    version(X86) assert(reflected.alignment == 4);
}

struct TestStruct {
    export int exportField;
    public int publicField;
    package int packageField;
    protected int protectedField;
    private int privateField;
}

//Test Struct
unittest {
    immutable StructDefinition reflected = reflectStruct!TestStruct;
    assert(reflected.fields.length == 5);
    assert(reflected.alignment == 4);
}

interface TestInterface {
    void publicMethod();
    void privateMethod();
}

unittest {
    immutable InterfaceDefinition reflected = reflectInterface!TestInterface;
    assert(reflected.methods.length == 2);
}

/*
T templateFunction(T)(T value) { return value; }

//Test Templated Functions
unittest {
    immutable FunctionDefinition reflected = reflectFunction!(templateFunction!int);
}
*/
