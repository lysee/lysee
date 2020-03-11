{==============================================================================}
{        UNIT: lysee_md5_funcs                                                 }
{ DESCRIPTION: calculate md5 digiest                                           }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2011/07/18                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmmd5;

{$mode objfpc}{$H+}

interface

{@pmc-description calculate md5 digiest}
function md5sum(const S: string; sumFile: boolean): string;
function md5sums(const S: string): string;
function md5sumf(const FileName: string): string;
{@pmc-end}

implementation

uses
  md5;

function md5sum(const S: string; sumFile: boolean): string;
begin
  if sumFile then
    Result := MD5Print(MD5File(S)) else
    Result := MD5Print(MD5String(S));
end;

function md5sums(const S: string): string;
begin
  Result := md5sum(S, false);
end;

function md5sumf(const FileName: string): string;
begin
  Result := md5sum(FileName, true);
end;

end.

