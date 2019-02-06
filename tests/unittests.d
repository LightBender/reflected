module tests.unittests;

import reflected;

public enum TestEnum {
    Value1 = 1,
    Value2 = 2,
    Value100 = 100,
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
    //package int packageField;
    //protected int protectedField;
    //private int privateField;


}

//Test Class
unittest {
    immutable ClassDefinition reflected = reflectClass!TestClass;
}

struct TestStruct {
    export int exportField;
    public int publicField;
    package int packageField;
}

unittest {
    immutable StructDefinition reflected = reflectStruct!TestStruct;
}