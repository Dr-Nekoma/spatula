ldstr "Hello World"
call void [mscorlib]System.Console::WriteLine(class System.Object).assembly extern mscorlib {}
.assembly test {}.method static public void main(void) cil managed {
.entrypoint
.maxstack 10000
ldstr "abc"
call void [mscorlib]System.Console::WriteLine(class System.Object)
ret
}