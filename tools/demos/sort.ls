uses sh;

l := [14, 55, 0, 3, 86, 20, 27, 67, 31, 16, 37, 42];
writeln("Sort " + l + " with:");

e := [,];
s := findFile(filePath(__file) + "*_sort.ls");
while s.active() > 0 do
    e.add(s.fullName());
    writeln("    " + length(e) + ". " + s.name().replace("_sort.ls", " sort"));
    s.next();

write(">>> ");
x := (readln().trim() as int) - 1;
if (x >= 0) and (x < length(e)) then
    writeln("Sort by " + e[x]);
    writeln("===> " + ::uses("", e[x]).sort(l));




