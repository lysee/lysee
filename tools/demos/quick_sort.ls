procedure do_sort(list, l, r);
    i := l;
    j := r;
    p := (l + r) shr 1;
    repeat
        while list[i] < list[p] do i += 1;
        while list[j] > list[p] do j -= 1;
        if i <= j then
            list.exchange(i, j);
            if p = i then p = j;
            elif p = j then p = i;
            i += 1;
            j -= 1;
        until i > j;
    if l < j then do_sort(list, l, j);
    l := i;
    if i < r then continue(list, l, r);

function sort(list: varlist): varlist;
    n := length(list);
    if n > 1 then
        do_sort(list, 0, n - 1);
    result := list;

if __in_main then
    writeln(sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]));

