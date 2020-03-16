class TSort()
    // Bubble sort function
    def Bubble(vals)
        var list := TList();
        list.Assign(vals);
        var n := list.Count;
        for i := 0 to n - 2 do
            for j := i + 1 to n - 1 do
                if list[i] > list[j] then
                    list.exchange(i, j);
        Result := list;
        
    // Quick sort function         
    def quick_sort(list, l, r);
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
        if l < j then self.quick_sort(list, l, j);
        l := i;
        if i < r then self.quick_sort(list, l, r);

    def Quick(vals)
        var list := TList();
        list.Assign(vals);
        var n := list.Count;
        if n > 1 then
            self.quick_sort(list, 0, n - 1);
        Result := list;
        
if __in_main_func then
    V := [2.3, 1.3, 15.02, 25.02, 45, 85.14, 56.1, 35.2, 4.2, 15.4];
    Writeln('Sort: ' + V);
    
    S := TSort();
    
    L := S.Bubble(V);
    Writeln('Bubble: ' + L);
    
    L := S.Quick(V);
    Writeln('Quick: ' + L);

