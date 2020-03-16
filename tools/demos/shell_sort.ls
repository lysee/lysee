uses math;

function sort(list: varlist): varlist;
    gap := length(list);
    while gap > 1 do
        gap := round(gap / 2);
        for i := gap to length(list) - 1 do
            j := i;
            while j > 0 do
                if list[j] <= list[j - gap] then
                    list.exchange(j,  j - gap);
                j -= gap;
    result := list;

if __in_main then
    writeln(sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]));

