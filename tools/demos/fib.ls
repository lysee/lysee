function fib(n: int): int;
    if n < 2 then
        result := n;
    else
        result := fib(n - 2) + fib(n - 1);

if __in_main then
    writeln(fib(20));

