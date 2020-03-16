uses math;

function merge(l: varlist, r: varlist): varlist;
    result := [];
    while (length(l) > 0) and (length(r) > 0) do
        if l[0] < r[0] then
            result.add(l[0]);
            l.delete(0);
        else
            result.add(r[0]);
            r.delete(0);
    result <+ l;
    result <+ r;

function sort(list: varlist): varlist;
    n := length(list);
    if n < 2 then exit(list);
    m := round(n / 2);
    l := sort(list.left(m));
    r := sort(list.right(n - m));
    result := merge(l, r);

if __in_main then
    writeln(sort([2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4]));

