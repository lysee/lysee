function sort(list: varlist): varlist;
    n := length(list);
    if n > 1 then
        for i := 0 to n - 1 do
            v := list[i];
            for j := i - 1 downto 0 do
                if list[j] <= v then break;
                list[j + 1] := list[j];
            list[j + 1] := v;
    result := list;

if __in_main then
    writeln(sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]));
