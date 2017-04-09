unit c_rtl;

interface

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

uses
  Classes,
  SysUtils;

const
  msvcrt = 'msvcrt.dll';

type
{$IFDEF NEXTGEN}
  PAnsiChar = MarshaledAString;
  PPAnsiChar = ^PAnsiChar;
{$ENDIF}

 {$IFDEF WIN64}
  size_t = Int64;
  {$ELSE}
  size_t = integer;
{$ENDIF}
  uint=LongWord;
  short=Word;
  uint32_t = LongWord;
  uint64_t = Int64;

  PStream=^TMemoryStream;

  va_list = Pointer;

  qsort_compare_func = function(P1, P2: Pointer): Integer; cdecl;

  time_t = {$IFDEF Win32} Integer {$ENDIF}
           {$IFDEF Win64} Int64 {$ENDIF};
  Ptime_t = ^time_t;

  _time64_t = Int64;
  P_time64_t = ^_time64_t;

  tm = packed record
    tm_sec: Integer;            { Seconds.      [0-60] (1 leap second) }
    tm_min: Integer;            { Minutes.      [0-59]  }
    tm_hour: Integer;           { Hours.        [0-23]  }
    tm_mday: Integer;           { Day.          [1-31]  }
    tm_mon: Integer;            { Month.        [0-11]  }
    tm_year: Integer;           { Year          - 1900. }
    tm_wday: Integer;           { Day of week.  [0-6]   }
    tm_yday: Integer;           { Days in year. [0-365] }
    tm_isdst: Integer;          { DST.          [-1/0/1]}
  end;
  Ptm = ^tm;

  TFile = packed record
     level: Short;
     flags: LongWord;
     fd: Char;
     hold: Byte;
     bsize: Short;
     buffer: PAnsiChar;
     curp: PAnsiChar;
     istemp: LongWord;
     token: Short;
  end;

  PFile =^TFile;


{ ----------------------------------------------------- }
{       Memory                                          }
{ ----------------------------------------------------- }

function  malloc(size: size_t): Pointer; cdecl;
function realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
procedure  free(pBlock: Pointer); cdecl;

{ ----------------------------------------------------- }
{       CString                                         }
{ ----------------------------------------------------- }

function  memchr(s: Pointer; c: Integer; n: size_t): Pointer; cdecl;
function  memcmp(buf1: Pointer; buf2: Pointer; n: size_t): Integer; cdecl;
function  memcpy(dest, src: Pointer; count: size_t): Pointer; cdecl;
function  memmove(dest, src: Pointer; count: size_t): Pointer; cdecl;
function  memset(dest: Pointer; val: Integer; count: size_t): Pointer; cdecl;
function  strcat(dest: PAnsiChar; src: PAnsiChar): PAnsiChar; cdecl;
function  strcpy(dest, src: PAnsiChar): PAnsiChar; cdecl;
function  strncpy(dest, src: PAnsiChar; n: size_t): PAnsiChar; cdecl;
function  strcmp(s1: PAnsiChar; s2: PAnsiChar): Integer; cdecl;
function  strncmp(s1: PAnsiChar; s2: PAnsiChar; n: size_t): Integer; cdecl;
function  strlen(s: PAnsiChar): size_t; cdecl;
function  strnlen(s: PAnsiChar; n: size_t): size_t; cdecl;
function  strchr(__s: PAnsiChar; __c: Integer): PAnsiChar; cdecl;
function  strerror(__errnum: Integer): PAnsiChar; cdecl;
function strcspn(const str1, str2: PAnsiChar): size_t; cdecl;
function stricmp(const str1, str2: PAnsiChar): Integer; cdecl;
function _stricmp(const str1, str2: PAnsiChar): Integer; cdecl;
function _mbscspn(const str, strCharSet: PWideChar): size_t; cdecl;
function mbstowcs(pwcs: PWideChar; const s: PWideChar;n: size_t): size_t; cdecl;
function wcslen(str: PWideChar): size_t; cdecl;
function wcsnlen(str: PWideChar; n: size_t): size_t; cdecl;
function wcstombs(s:Pointer; const pwcs:Pointer; n:Integer):Integer; cdecl;
function strstr(const str1, str2: PAnsiChar): PAnsiChar; cdecl;
function wcscpy(dest, src: PWideChar): PWideChar; cdecl;

{ ----------------------------------------------------- }
{       Locale                                          }
{ ----------------------------------------------------- }

function  tolower(__ch: Integer): Integer; cdecl;
function  toupper(__ch: Integer): Integer; cdecl;
function  towlower(__ch: Integer): Integer; cdecl;
function  towupper(__ch: Integer): Integer; cdecl;
function  isalnum(__ch: Integer): Integer; cdecl;
function  isalpha(__ch: Integer): Integer; cdecl;
function  iscntrl(__ch: Integer): Integer; cdecl;
function  isdigit(__ch: Integer): Integer; cdecl;
function  isgraph(__ch: Integer): Integer; cdecl;
function  islower(__ch: Integer): Integer; cdecl;
function  isprint(__ch: Integer): Integer; cdecl;
function  ispunct(__ch: Integer): Integer; cdecl;
function  isspace(__ch: Integer): Integer; cdecl;
function  isupper(__ch: Integer): Integer; cdecl;
function  isxdigit(__ch: Integer): Integer; cdecl;
function _ismbblead(c: Cardinal): Integer; cdecl;

{ ----------------------------------------------------- }
{       IO                                              }
{ ----------------------------------------------------- }

function _open(const __path: PAnsiChar; __access: Integer; __permission: Integer): Integer; cdecl;
function _wopen(const __path: PChar; __access: Integer; __permission: Integer): Integer; cdecl;
function _close(__handle: Integer): Integer; cdecl;
function _lseek(__handle: Integer; __offset: Integer; __fromwhere: Integer): Integer; cdecl;
function _read(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
function _write(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
function open(const __path: PAnsiChar; __access: Integer; __permission: Integer): Integer; cdecl;
function close(__handle: Integer): Integer; cdecl;
function lseek(__handle: Integer; __offset: Integer; __fromwhere: Integer): Integer; cdecl;
function read(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;
function write(__handle: Integer; __buf: Pointer; __len: LongWord): Integer; cdecl;

{ ----------------------------------------------------- }
{       Standard IO                                     }
{ ----------------------------------------------------- }

function  printf(format: PAnsiChar {args}): Integer; cdecl; varargs;
function  fprintf(fHandle: Pointer; format: PAnsiChar {args}): Integer; cdecl; varargs;
function  sprintf(buf: Pointer; format: PAnsiChar {args}): Integer; cdecl; varargs;
function snprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
function _snprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
function vsnprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
function _vsnprintf(buf: Pointer; nzize: size_t; format: PAnsiChar; param: va_list): Integer; cdecl;
function _fwrite(buf: Pointer; size: size_t; count: size_t; stream: Pointer): size_t; cdecl;
function _fread(buf: Pointer; size: size_t; count: size_t; stream: Pointer): size_t; cdecl;
function _fflush(stream: Pointer): Integer; cdecl;

{ ----------------------------------------------------- }
{       Conversion                                      }
{ ----------------------------------------------------- }

function _itoa(value: Integer; str: PAnsiChar; radix: Integer): PAnsiChar; cdecl;
function itoa(value: Integer; str: PAnsiChar; radix: Integer): PAnsiChar; cdecl;
function _i64toa(value: Int64; str: PAnsiChar; radix: Integer): PAnsiChar; cdecl;
function _atoi64(const str: PAnsiChar): Int64; cdecl;
function atoi(const str: PAnsiChar): Integer; cdecl;
function atof(value: PAnsiChar): Double; cdecl;
function atol(const str: PAnsiChar): LongInt; cdecl;
function strtod(value: PAnsiChar; endPtr: PPAnsiChar): Double; cdecl;
function gcvt(value: double; digits: Integer; buffer: PAnsiChar): PAnsiChar; cdecl;
function _gcvt(value: double; digits: Integer; buffer: PAnsiChar): PAnsiChar; cdecl;

const
  _fltused: Integer = $9875;  // from stubs.c in MS crtl
  _streams: array [0..2] of NativeInt = (0, 1, 2);

var
  _errno: Integer;
  __errno: Integer;
  ___errno: Integer;
  __turboFloat: Integer = 0; // Win32
  __streams:Integer;


procedure _mbctype; // Not a function, pointer to data

{$IFDEF WIN64}
procedure _purecall; cdecl;
function _lseeki64(__handle: Integer; __offset: Int64; __fromwhere: Integer): Int64; cdecl;
{$ENDIF}

{$IFDEF WIN32}
procedure __pure_error_;
function GetMem2(Size: NativeInt): Pointer;
function SysFreeMem2(p: Pointer): Integer;
function _malloc(size: size_t): Pointer; cdecl;
function _realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
procedure _free(pBlock: Pointer); cdecl;

function __atold(value: PAnsiChar; endPtr: PPAnsiChar): Extended; cdecl;
procedure _ftol; cdecl; external;
procedure __ftol; cdecl; external; {$L ftol.obj}
procedure _ftoul; cdecl;
procedure __ftoul; cdecl; external; {$L _ftoul.obj}

procedure __mbctype; // Not a function, pointer to data
function  _ltolower(__ch: Integer): Integer; cdecl;
function  _ltoupper(__ch: Integer): Integer; cdecl;
function _ltowlower(c:Integer):Integer; cdecl;
function _ltowupper(c:Integer):Integer; cdecl;
procedure __ltolower; cdecl;
procedure __ltoupper; cdecl;
procedure __ltowlower; cdecl;
procedure __ltowupper; cdecl;
procedure _atof; cdecl;
procedure _atol; cdecl;
procedure _strcspn; cdecl;
procedure _strcat; cdecl;
procedure _strcmp; cdecl;
procedure _strncmp; cdecl;
procedure _strcpy; cdecl;
procedure _strncpy; cdecl;
procedure _memmove; cdecl;
procedure _memset; cdecl;
procedure _memcpy; cdecl;
procedure _memcmp; cdecl;
procedure _memchr; cdecl;
procedure _strlen; cdecl;
procedure _islower; cdecl;
procedure _isdigit; cdecl;
procedure _isupper; cdecl;
procedure _isalnum; cdecl;
procedure _isspace; cdecl;
procedure _isxdigit; cdecl;
procedure _isgraph; cdecl;
procedure _isprint; cdecl;
procedure _ispunct; cdecl;
procedure _iscntrl; cdecl;
procedure _isalpha; cdecl;
procedure _strchr; cdecl;
procedure _strnlen; cdecl;
procedure _wcslen; cdecl;
procedure _wcsnlen; cdecl;
procedure _printf; cdecl;
procedure _fprintf; cdecl;
procedure _sprintf; cdecl;
procedure __vsnprintf; cdecl;
procedure _tolower; cdecl;
procedure _toupper; cdecl;
procedure __mbscspn; cdecl;
procedure __i64toa; cdecl;
procedure __atoi64; cdecl;
procedure _strstr; cdecl;
procedure _mbstowcs; cdecl;
procedure _wcstombs; cdecl;
procedure _strerror; cdecl;
procedure _llmod; cdecl;
procedure _lldiv; cdecl;
procedure _lludiv; cdecl;
procedure _llmul; cdecl;
procedure _llumod; cdecl;
procedure _llshl; cdecl;
procedure _llshr; cdecl;
procedure _llushr; cdecl;
{$ENDIF WIN32}

procedure qsort(baseP: PByte; NElem, Width: size_t; comparF: qsort_compare_func); cdecl;
function localtime(t: Ptime_t): Ptm; cdecl;

function _beginthreadex(security_attr: Pointer; stksize: LongWord;
  start: Pointer; arg: Pointer; create_flags: LongWord;
  var thread_id: LongWord): LongWord; cdecl;
procedure _endthreadex(thread_retval: LongWord);

procedure _exit( status:integer ); cdecl;

implementation


{ ----------------------------------------------------- }
{       Memory                                          }
{ ----------------------------------------------------- }

function  malloc(size: size_t): Pointer; cdecl;
begin
  Result := AllocMem(size);
end;

function realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
begin
  ReallocMem(P, Newsize);
  Result := P;
end;

procedure free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;

{$IFDEF WIN64}
procedure _purecall; cdecl;
asm
  jmp System.@AbstractError
end;

function _lseeki64; external msvcrt;
{$ENDIF}

{$IFDEF WIN32}
procedure _llmod; cdecl;
asm
  jmp System.@_llmod;
end;

procedure _lldiv; cdecl;
asm
  jmp System.@_lldiv
end;

procedure _lludiv; cdecl;
asm
  jmp System.@_lludiv
end;

procedure _llmul; cdecl;
asm
  jmp System.@_llmul
end;

procedure _llumod; cdecl;
asm
  jmp System.@_llumod
end;

procedure _llshl; cdecl;
asm
  jmp System.@_llshl
end;

procedure _llshr; cdecl;
asm
        AND   CL, $3F
        CMP   CL, 32
        JL    @__llshr@below32
        MOV   EAX, EDX
        CDQ
        SAR   EAX,CL
        RET

@__llshr@below32:
        SHRD  EAX, EDX, CL
        SAR   EDX, CL
        RET
end;

procedure _llushr; cdecl;
asm
  jmp System.@_llushr
end;

function _malloc(size: size_t): Pointer; cdecl;
begin
  try
    Result := AllocMem(size);
  except
    Result := nil;
  end;
end;

function _realloc(P: Pointer; NewSize: size_t): Pointer; cdecl;
begin
  try
    ReallocMem(P, Newsize);
    Result := P;
  except
    Result := nil;
  end;
end;

procedure _free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;

procedure __pure_error_;
asm
  JMP  System.@AbstractError
end;

// C++'s alloc allocates 1 byte if size is 0.
function GetMem2(Size: NativeInt): Pointer;
begin
  if Size = 0 then Inc(Size);
  GetMem(Result, Size);
end;

// C++'s free allow NULL pointer.
function SysFreeMem2(p: Pointer): Integer;
begin
  result := 0;
  if (p <> NIL) then result := FreeMemory(p);
end;

function __atold(value: PAnsiChar; endPtr: PPAnsiChar): Extended; cdecl;
var
  s: string;
begin
  s := string(Value);
  if endPtr <> nil then
    endPtr^ := value;
  if not TryStrToFloat(s, Result) then
    Result := 0
  else if endPtr <> nil then
    endPtr^ := PAnsiChar(PByte(Value) + Length(s));
end;

procedure _ftoul; cdecl;
asm
  JMP  System.@Trunc
end;
{$ENDIF WIN32}

{ ----------------------------------------------------- }
{       CString                                         }
{ ----------------------------------------------------- }

function  memchr; external msvcrt;
function  memcmp; external msvcrt;
function  memcpy; external msvcrt;
function  memmove; external msvcrt;
function  memset; external msvcrt;
function  strcat; external msvcrt;
function  strcpy; external msvcrt;
function  strncpy; external msvcrt;
function  strcmp; external msvcrt;
function  strncmp; external msvcrt;
function  strlen; external msvcrt;
function  strnlen; external msvcrt;
function  strchr; external msvcrt;
function  strerror; external msvcrt;
function strcspn; external msvcrt;
function stricmp; external msvcrt name '_stricmp';
function _stricmp; external msvcrt;
function _mbscspn; external msvcrt;
function mbstowcs; external msvcrt;
function wcslen; external msvcrt;
function wcsnlen; external msvcrt;
function wcstombs; external msvcrt;
function strstr; external msvcrt;
function wcscpy; external msvcrt;

{ ----------------------------------------------------- }
{       Locale                                          }
{ ----------------------------------------------------- }

function  tolower; external msvcrt;
function  toupper; external msvcrt;
function  towlower; external msvcrt;
function  towupper; external msvcrt;
function  isalnum; external msvcrt;
function  isalpha; external msvcrt;
function  iscntrl; external msvcrt;
function  isdigit; external msvcrt;
function  isgraph; external msvcrt;
function  islower; external msvcrt;
function  isprint; external msvcrt;
function  ispunct; external msvcrt;
function  isspace; external msvcrt;
function  isupper; external msvcrt;
function  isxdigit; external msvcrt;
function _ismbblead; external msvcrt;

{ ----------------------------------------------------- }
{       IO                                              }
{ ----------------------------------------------------- }

function _wopen; external msvcrt;
function _open; external msvcrt;
function _close; external msvcrt;
function _lseek; external msvcrt;
function _read; external msvcrt;
function _write; external msvcrt;
function open; external msvcrt name '_open';
function close; external msvcrt name '_close';
function lseek; external msvcrt name '_lseek';
function read; external msvcrt name '_read';
function write; external msvcrt name '_write';

{ ----------------------------------------------------- }
{       Standard IO                                     }
{ ----------------------------------------------------- }

function  printf; external msvcrt;
function  fprintf; external msvcrt;
function  sprintf; external msvcrt;
function snprintf; external msvcrt name '_snprintf';
function _snprintf; external msvcrt;
function vsnprintf; external msvcrt name '_vsnprintf';
function _vsnprintf; external msvcrt;
function _fwrite; external msvcrt name 'fwrite';
function _fread; external msvcrt name 'fread';
function _fflush; external msvcrt name 'fflush';

{ ----------------------------------------------------- }
{       Conversion                                      }
{ ----------------------------------------------------- }

function _itoa; external msvcrt;
function itoa; external msvcrt name '_itoa';
function _i64toa; external msvcrt;
function _atoi64; external msvcrt;
function atoi; external msvcrt;
function atof; external msvcrt;
function atol; external msvcrt;
function strtod; external msvcrt;
function gcvt; external msvcrt name '_gcvt';
function _gcvt; external msvcrt;
procedure _mbctype; external msvcrt; // Not a function, pointer to data

{$IFDEF WIN32}
procedure __mbctype; external msvcrt name '_mbctype'; // Not a function, pointer to data
function  _ltolower; external msvcrt name 'tolower';
function  _ltoupper; external msvcrt name 'toupper';
function _ltowlower; external msvcrt name 'towlower';
function _ltowupper; external msvcrt name 'towupper';
procedure __ltolower; external msvcrt name 'tolower';
procedure __ltoupper; external msvcrt name 'toupper';
procedure __ltowlower; external msvcrt name 'towlower';
procedure __ltowupper; external msvcrt name 'towupper';
procedure _atof; external msvcrt name 'atof';
procedure _atol; external msvcrt name 'atol';
procedure _strcspn; external msvcrt name 'strcspn';
procedure _strcat; external msvcrt name 'strcat';
procedure _strcmp; external msvcrt name 'strcmp';
procedure _strncmp; external msvcrt name 'strncmp';
procedure _strcpy; external msvcrt name 'strcpy';
procedure _strncpy; external msvcrt name 'strncpy';
procedure _memmove; external msvcrt name 'memmove';
procedure _memset; external msvcrt name 'memset';
procedure _memcpy; external msvcrt name 'memcpy';
procedure _memcmp; external msvcrt name 'memcmp';
procedure _memchr; external msvcrt name 'memchr';
procedure _strlen; external msvcrt name 'strlen';
procedure _islower; external msvcrt name 'islower';
procedure _isdigit; external msvcrt name 'isdigit';
procedure _isupper; external msvcrt name 'isupper';
procedure _isalnum; external msvcrt name 'isalnum';
procedure _isspace; external msvcrt name 'isspace';
procedure _isxdigit; external msvcrt name 'isxdigit';
procedure _isgraph; external msvcrt name 'isgraph';
procedure _isprint; external msvcrt name 'isprint';
procedure _ispunct; external msvcrt name 'ispunct';
procedure _iscntrl; external msvcrt name 'iscntrl';
procedure _isalpha; external msvcrt name 'isalpha';
procedure _strchr; external msvcrt name 'strchr';
procedure _strnlen; external msvcrt name 'strnlen';
procedure _wcslen; external msvcrt name 'wcslen';
procedure _wcsnlen; external msvcrt name 'wcsnlen';
procedure _printf; external msvcrt name 'printf';
procedure _fprintf; external msvcrt name 'fprintf';
procedure _sprintf; external msvcrt name 'sprintf';
procedure __vsnprintf; external msvcrt name '_vsnprintf';
procedure _tolower; external msvcrt name 'tolower';
procedure _toupper; external msvcrt name 'toupper';
procedure __mbscspn; external msvcrt name '_mbscspn';
procedure __i64toa; external msvcrt name '_i64toa';
procedure __atoi64; external msvcrt name '_atoi64';
procedure _strstr; external msvcrt name 'strstr';
procedure _mbstowcs; external msvcrt name 'mbstowcs';
procedure _wcstombs; external msvcrt name 'wcstombs';
procedure _strerror; external msvcrt name 'strerror';
{$ENDIF WIN32}

procedure qsort; external msvcrt;
function localtime; external msvcrt;
function _beginthreadex; external msvcrt;
procedure _endthreadex; external msvcrt;

procedure _exit(status:integer); external msvcrt name '_exit';

end.
