package com.example;


public class Test{
    public static void main(String[] args) {
        B b = new B();
        System.out.println(!(b instanceof C));

    }
}

class A {
}

class B extends A{

}

class C extends B{

}


