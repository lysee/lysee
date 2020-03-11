{==============================================================================}
{     PROJECT: lysee_hz                                                        }
{ DESCRIPTION: functions of chinese word                                       }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2007/07/12                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmhz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{@pmc-description pinyin of chinese language}
function pinyin(const chinese: string; longFmt, separateLongFmt: boolean): string;
{@pmc-end}

implementation

const
  PY_COUNT = 396;

  PY_VALUE: array[0..PY_COUNT - 1] of word = (
    $B0A1, $B0A3, $B0B0, $B0B9, $B0BC, $B0C5, $B0D7, $B0DF, $B0EE, $B0FA,
    $B1AD, $B1BC, $B1C0, $B1C6, $B1DE, $B1EA, $B1EE, $B1F2, $B1F8, $B2A3,
    $B2B8, $B2C1, $B2C2, $B2CD, $B2D4, $B2D9, $B2DE, $B2E3, $B2E5, $B2F0,
    $B2F3, $B2FD, $B3AC, $B3B5, $B3BB, $B3C5, $B3D4, $B3E4, $B3E9, $B3F5,
    $B4A7, $B4A8, $B4AF, $B4B5, $B4BA, $B4C1, $B4C3, $B4CF, $B4D5, $B4D6,
    $B4DA, $B4DD, $B4E5, $B4E8, $B4EE, $B4F4, $B5A2, $B5B1, $B5B6, $B5C2,
    $B5C5, $B5CC, $B5DF, $B5EF, $B5F8, $B6A1, $B6AA, $B6AB, $B6B5, $B6BC,
    $B6CB, $B6D1, $B6D5, $B6DE, $B6EA, $B6F7, $B6F8, $B7A2, $B7AA, $B7BB,
    $B7C6, $B7D2, $B7E1, $B7F0, $B7F1, $B7F2, $B8C1, $B8C3, $B8C9, $B8D4,
    $B8DD, $B8E7, $B8F8, $B8F9, $B8FB, $B9A4, $B9B3, $B9BC, $B9CE, $B9D4,
    $B9D7, $B9E2, $B9E5, $B9F5, $B9F8, $B9FE, $BAA1, $BAA8, $BABB, $BABE,
    $BAC7, $BAD9, $BADB, $BADF, $BAE4, $BAED, $BAF4, $BBA8, $BBB1, $BBB6,
    $BBC4, $BBD2, $BBE7, $BBED, $BBF7, $BCCE, $BCDF, $BDA9, $BDB6, $BDD2,
    $BDED, $BEA3, $BEBC, $BEBE, $BECF, $BEE8, $BEEF, $BEF9, $BFA6, $BFAA,
    $BFAF, $BFB5, $BFBC, $BFC0, $BFCF, $BFD3, $BFD5, $BFD9, $BFDD, $BFE4,
    $BFE9, $BFED, $BFEF, $BFF7, $C0A4, $C0A8, $C0AC, $C0B3, $C0B6, $C0C5,
    $C0CC, $C0D5, $C0D7, $C0E2, $C0E5, $C1A9, $C1AA, $C1B8, $C1C3, $C1D0,
    $C1D5, $C1E1, $C1EF, $C1FA, $C2A5, $C2AB, $C2BF, $C2CD, $C2D3, $C2D5,
    $C2DC, $C2E8, $C2F1, $C2F7, $C3A2, $C3A8, $C3B4, $C3B5, $C3C5, $C3C8,
    $C3D0, $C3DE, $C3E7, $C3EF, $C3F1, $C3F7, $C3FD, $C3FE, $C4B1, $C4B4,
    $C4C3, $C4CA, $C4CF, $C4D2, $C4D3, $C4D8, $C4D9, $C4DB, $C4DC, $C4DD,
    $C4E8, $C4EF, $C4F1, $C4F3, $C4FA, $C4FB, $C5A3, $C5A7, $C5AB, $C5AE,
    $C5AF, $C5B0, $C5B2, $C5B6, $C5B7, $C5BE, $C5C4, $C5CA, $C5D2, $C5D7,
    $C5DE, $C5E7, $C5E9, $C5F7, $C6AA, $C6AE, $C6B2, $C6B4, $C6B9, $C6C2,
    $C6CB, $C6DA, $C6FE, $C7A3, $C7B9, $C7C1, $C7D0, $C7D5, $C7E0, $C7ED,
    $C7EF, $C7F7, $C8A6, $C8B1, $C8B9, $C8BB, $C8BF, $C8C4, $C8C7, $C8C9,
    $C8D3, $C8D5, $C8D6, $C8E0, $C8E3, $C8ED, $C8EF, $C8F2, $C8F4, $C8F6,
    $C8F9, $C8FD, $C9A3, $C9A6, $C9AA, $C9AD, $C9AE, $C9AF, $C9B8, $C9BA,
    $C9CA, $C9D2, $C9DD, $C9E9, $C9F9, $CAA6, $CAD5, $CADF, $CBA2, $CBA4,
    $CBA8, $CBAA, $CBAD, $CBB1, $CBB5, $CBB9, $CBC9, $CBD1, $CBD4, $CBE1,
    $CBE4, $CBEF, $CBF2, $CBFA, $CCA5, $CCAE, $CCC0, $CCCD, $CCD8, $CCD9,
    $CCDD, $CCEC, $CCF4, $CCF9, $CCFC, $CDA8, $CDB5, $CDB9, $CDC4, $CDC6,
    $CDCC, $CDCF, $CDDA, $CDE1, $CDE3, $CDF4, $CDFE, $CEC1, $CECB, $CECE,
    $CED7, $CEF4, $CFB9, $CFC6, $CFE0, $CFF4, $D0A8, $D0BD, $D0C7, $D0D6,
    $D0DD, $D0E6, $D0F9, $D1A5, $D1AB, $D1B9, $D1C9, $D1EA, $D1FB, $D2AC,
    $D2BB, $D2F0, $D3A2, $D3B4, $D3B5, $D3C4, $D3D9, $D4A7, $D4BB, $D4C5,
    $D4D1, $D4D4, $D4DB, $D4DF, $D4E2, $D4F0, $D4F4, $D4F5, $D4F6, $D4FA,
    $D5AA, $D5B0, $D5C1, $D5D0, $D5DA, $D5E4, $D5F4, $D6A5, $D6D0, $D6DB,
    $D6E9, $D7A5, $D7A7, $D7A8, $D7AE, $D7B5, $D7BB, $D7BD, $D7C8, $D7D7,
    $D7DE, $D7E2, $D7EA, $D7EC, $D7F0, $D7F2
  );

  PY_SPELL: array[0..PY_COUNT - 1] of string = (
    'a','ai','an','ang','ao','ba','bai','ban','bang','bao','bei','ben','beng',
    'bi','bian','biao','bie','bin','bing','bo','bu','ca','cai','can','cang',
    'cao','ce','ceng','cha','chai','chan','chang','chao','che','chen','cheng',
    'chi','chong','chou','chu','chuai','chuan','chuang','chui','chun','chuo',
    'ci','cong','cou','cu','cuan','cui','cun','cuo','da','dai','dan','dang',
    'dao','de','deng','di','dian','diao','die','ding','diu','dong','dou','du',
    'duan','dui','dun','duo','e','en','er','fa','fan','fang','fei','fen','feng',
    'fo','fou','fu','ga','gai','gan','gang','gao','ge','gei','gen','geng',
    'gong','gou','gu','gua','guai','guan','guang','gui','gun','guo','ha','hai',
    'han','hang','hao','he','hei','hen','heng','hong','hou','hu','hua','huai',
    'huan','huang','hui','hun','huo','ji','jia','jian','jiang','jiao','jie',
    'jin','jing','jiong','jiu','ju','juan','jue','jun','ka','kai','kan','kang',
    'kao','ke','ken','keng','kong','kou','ku','kua','kuai','kuan','kuang','kui',
    'kun','kuo','la','lai','lan','lang','lao','le','lei','leng','li','lia',
    'lian','liang','liao','lie','lin','ling','liu','long','lou','lu','lv',
    'luan','lue','lun','luo','ma','mai','man','mang','mao','me','mei','men',
    'meng','mi','mian','miao','mie','min','ming','miu','mo','mou','mu','na',
    'nai','nan','nang','nao','ne','nei','nen','neng','ni','nian','niang','niao',
    'nie','nin','ning','niu','nong','nu','nv','nuan','nue','nuo','o','ou','pa',
    'pai','pan','pang','pao','pei','pen','peng','pi','pian','piao','pie','pin',
    'ping','po','pu','qi','qia','qian','qiang','qiao','qie','qin','qing',
    'qiong','qiu','qu','quan','que','qun','ran','rang','rao','re','ren','reng',
    'ri','rong','rou','ru','ruan','rui','run','ruo','sa','sai','san','sang',
    'sao','se','sen','seng','sha','shai','shan','shang','shao','she','shen',
    'sheng','shi','shou','shu','shua','shuai','shuan','shuang','shui','shun',
    'shuo','si','song','sou','su','suan','sui','sun','suo','ta','tai','tan',
    'tang','tao','te','teng','ti','tian','tiao','tie','ting','tong','tou','tu',
    'tuan','tui','tun','tuo','wa','wai','wan','wang','wei','wen','weng','wo',
    'wu','xi','xia','xian','xiang','xiao','xie','xin','xing','xiong','xiu','xu',
    'xuan','xue','xun','ya','yan','yang','yao','ye','yi','yin','ying','yo',
    'yong','you','yu','yuan','yue','yun','za','zai','zan','zang','zao','ze',
    'zei','zen','zeng','zha','zhai','zhan','zhang','zhao','zhe','zhen','zheng',
    'zhi','zhong','zhou','zhu','zhua','zhuai','zhuan','zhuang','zhui','zhun',
    'zhuo','zi','zong','zou','zu','zuan','zui','zun','zuo'
  );

function pinyinHead(const S: string): string;
var
  index: integer;
  order: word;
begin
  Result := '';
  index := 1;
  while index < Length(S) do
  begin
    if S[index] in LeadBytes then
    begin
      order := word(S[index]) shl 8;
      Inc(index);
      if S[index] in LeadBytes then
        case order + word(S[index]) of
          $B0A1..$B0C4 : Result := Result + 'A';
          $B0C5..$B2C0 : Result := Result + 'B';
          $B2C1..$B4ED : Result := Result + 'C';
          $B4EE..$B6E9 : Result := Result + 'D';
          $B6EA..$B7A1 : Result := Result + 'E';
          $B7A2..$B8C0 : Result := Result + 'F';
          $B8C1..$B9FD : Result := Result + 'G';
          $B9FE..$BBF6 : Result := Result + 'H';
          $BBF7..$BFA5 : Result := Result + 'J';
          $BFA6..$C0AB : Result := Result + 'K';
          $C0AC..$C2E7 : Result := Result + 'L';
          $C2E8..$C4C2 : Result := Result + 'M';
          $C4C3..$C5B5 : Result := Result + 'N';
          $C5B6..$C5BD : Result := Result + 'O';
          $C5BE..$C6D9 : Result := Result + 'P';
          $C6DA..$C8BA : Result := Result + 'Q';
          $C8BB..$C8F5 : Result := Result + 'R';
          $C8F6..$CBF9 : Result := Result + 'S';
          $CBFA..$CDD9 : Result := Result + 'T';
          $CDDA..$CEF3 : Result := Result + 'W';
          $CEF4..$D188 : Result := Result + 'X';
          $D1B9..$D4D0 : Result := Result + 'Y';
          $D4D1..$D7F9 : Result := Result + 'Z';
        end;
    end
    else Result := Result + S[index];
    Inc(index);
  end;
end;

function pinyinLong(const S: string; SeparateHz: boolean): string;
var
  index, X: integer;
  order: word;
  hz, kg: boolean;
begin
  Result := '';
  index := 1;
  hz := false;
  kg := true;
  while index < Length(S) do
  begin
    if S[index] in LeadBytes then
    begin
      order := word(S[index]) shl 8;
      Inc(index);
      if S[index] in LeadBytes then
      begin
        order := order + word(S[index]);
        for X := PY_COUNT - 1 downto 0 do
          if PY_VALUE[X] < order then
          begin
            if SeparateHz and not kg then
              Result := Result + ' ' + PY_SPELL[X] else
              Result := Result + PY_SPELL[X];
            kg := false;
            hz := true;
            break;
          end;
      end;
    end
    else
    begin
      kg := S[index] in [#$09, #$0A, #$0C, #$0D, #$20];
      if SeparateHz and not kg and hz then
        Result := Result + ' ' + S[index] else
        Result := Result + S[index];
      hz := false;
    end;
    Inc(index);
  end;
end;

function pinyin(const chinese: string; longFmt, separateLongFmt: boolean): string;
var
  S: string;
begin
  S := Trim(chinese);
  if S = '' then Result := '' else
  if longFmt then
    Result := pinyinLong(S, separateLongFmt) else
    Result := pinyinHead(S);
end;

end.

