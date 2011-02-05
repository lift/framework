package net.liftweb.util;

public class FooTester {
    public static void test(BaseFoo foo) {
        foo.testInt(42);
    }

    public static void test(ComplexFoo foo) {
        foo.testString("foo!");
    }
}