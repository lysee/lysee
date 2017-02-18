{==============================================================================}
{     PROJECT: lysee_cmd                                                       }
{ DESCRIPTION: lysee script interpreter                                        }
{   COPYRIGHT: Copyright (c) 2003-2015, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2016/11/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
program lysee_cmd;

{$mode objfpc}{$H+}

uses
  basic, lysee, lysee_db;

begin
  lysee.Command;
end.

