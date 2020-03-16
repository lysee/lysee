function pop_min(list);
    v := list[0];
    x := 0;
    n := length(list);
    if n > 1 then
        for i := 1 to n - 1 do
            if list[i] < v then
                v := list[i];
                x := i;
    delete(list, x);
    result := v;

function sort(list: varlist): varlist;
    n := length(list);
    b := list.copy(0, n);
    list.clear();
    for x := 0 to n - 1 do
        list.add(pop_min(b));
    result := list;

if __in_main then
    writeln(sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]));

