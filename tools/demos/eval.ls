
def Interprete()
  writeln('Input expression to evaluate or \"quit\" to leave!');
  write(sLineBreak);
  while true do
    try
      write('lysee> ');
      input := Trim(readln);
      if input <> '' then
        if LowerCase(input) = 'quit' then break;
        writeln(eval('Exit(' + input + ')'));
    except
      writeln(__error.Msg);


if __in_main_func then
  Interprete();
  writeln('bye-bye!');

