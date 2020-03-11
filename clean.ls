#! lysee.exe
//
// check file ext
//
def is_target_file(fname)
    var ext := LowerCase(ExtractFileExt(fname)); 
    Result := (ext in ['.dcu', '.exe', '.dll', '.dcu', '.ppu', '.local', '.bak', 
                       '.identcache', '.stat', '.tds', '.o', '.compiled', 
                       '.obj', '.res', '.lps', '.dbg']) or ('~' in ext);
  
//
// find sub directorie and files
// 
def find_path(path, path_list, file_list)
    var S := TSearchRec();
    if S.FindFirst(path + '*.*', faAnyFile) then
        repeat
            if S.IsDirectory then
                path_list.Add(LowerCase(S.FileName));
                find_path(S.FileName + PathDelim, path_list, file_list);
            else
                file_list.Add(S.FileName);
        until not S.FindNext;
        S.FindClose;
           
//
// remove whole directory
//
def remove_dir(dir)
    dir := SetPathDelimiter(dir);
    Writeln('RMDIR ' + dir); 
    var path_list := TList();
    var file_list := TList();
    find_path(dir + PathDelim, path_list, file_list);
    for F in file_list do
        Writeln('DEL ' + F + ' -- ' + DeleteFile(F));
    for I := path_list.Count - 1 downto 0 do
        F := path_list[I];
        Writeln('RMV ' + F + ' -- ' + RemoveDir(F));
    Writeln('RMV ' + dir + ' -- ' + RemoveDir(dir));

if __in_main_func then
    var path := ExtractFilePath(__file); 
    var path_list := TList();
    var file_list := TList();
    find_path(path, path_list, file_list);

    for F in file_list do
        if is_target_file(F) then
            Writeln('DEL ' + F + ' -- ' + DeleteFile(F));

    remove_dir(path + 'tools/lib');
    remove_dir(path + 'tools/backup');
    remove_dir(path + 'tools/modules/lib');
    remove_dir(path + 'tools/modules/backup');
    remove_dir(path + 'tools/delphi/win32');    
    remove_dir(path + 'tools/delphi/backup');    
    remove_dir(path + 'tools/delphi/__history');    
    remove_dir(path + 'tools/delphi/__recovery');    
    remove_dir(path + 'tools/delphi/myrv/win32');    
    remove_dir(path + 'tools/delphi/myrv/backup');    
    remove_dir(path + 'tools/delphi/myrv/__history');    
    remove_dir(path + 'tools/delphi/myrv/__recovery');    
    remove_dir(path + 'source/backup');    
    remove_dir(path + 'source/__history');    
    remove_dir(path + 'source/__recovery'); 
       
    = 'Cleaned, press ENTER to quit: ';
    Readln;
 
