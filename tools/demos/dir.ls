uses sh;
write("List files of Pectory: ");
p := incPD(fullFileName(readln().trim()));
sr := findFile(p + "*.*");
if sr then
    repeat
        writeln(sr.fullName());
        until sr.next() = 0;

