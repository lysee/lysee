function Sort(list);
    n := length(list);
    for i := 0 to n - 2 do
        for j := i + 1 to n - 1 do
            if list[i] > list[j] then
                list.exchange(i, j);
    result := list;

if __in_main then
  writeln(Sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]));

