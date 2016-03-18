module profile_example;

int method1()
{
return 1;
}

int method2()
{
return method1 + method1;
}

int method3()
{
return method2 + method1;
}

int main(string[] args)
{
return method3;
}

