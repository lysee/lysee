procedure move(From, To);
    writeln(format("move %s to %s", [From, To]));

procedure hanoi(n, From, middle, To);
    if n = 1 then
        move(From, To);
    else
        hanoi(n - 1, From, To, middle);
        move(From, To);
        hanoi(n - 1, middle, From, To);

if __in_main then
    hanoi(5, "A", "B", "C");
