{==============================================================================}
{     PROJECT: lysee_pmc                                                       }
{ DESCRIPTION: pascal module compilter (PMC)                                   }
{   COPYRIGHT: Copyright (c) 2012, Li Yun Jie. All Rights Reserved.            }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2012/03/07                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmpmc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lpmc;

{@pmc-description pascal module compiler (PMC)}
{@pmc-funcs
  function build(const pasFile, options: string): string;
  function wrap(const pasFile: string; register: boolean): string;
}

implementation

end.

