/*
	DMANGLE.D
	---------
	Copyright (c) 2015 eBay Software Foundation
	Written by Andrew Trotman
	Licensed under the 3-clause BSD license (see here:https://en.wikipedia.org/wiki/BSD_licenses)

	The D runtime name demangle routine appears to be unable to demangle some names, such as those
	that contain lambdas.  This module is written as an exploration of D name demangling.
*/
module demangle;
import std.ascii;
import std.stdio;
import std.conv;
import std.format;
import std.string;
import std.array;

const (char) *source;
const (char) *end;
size_t peek_operations;
size_t max_allowable_peek_operations;

/*
	GET_NEXT()
	----------
*/
char get_next()
	{
	char ch = source < end ? *source++ : 0;
//	writeln("{", ch, "}");
	return ch;
	}

/*
	PEEK_NEXT()
	-----------
*/
char peek_next(size_t distance = 0)
	{
	if (peek_operations++ > max_allowable_peek_operations)
		throw new Exception("Infinate Loop (badly formed mangled name)");
	char ch =  source + distance < end ? *(source + distance) : 0;
//	writeln("[", ch, "]");
	return ch;
	}

/*
	NUMBER()
	--------
	Number:
		Digit
		Digit Number

	Digit:
		0
		1
		2
		3
		4
		5
		6
		7
		8
		9
*/
size_t number(size_t max_digits = size_t.max)
	{
	size_t answer = 0;

	while (isDigit(peek_next()) && max_digits-- > 0)
		answer = answer * 10 + get_next() - '0';

	return answer;
	}

/*
	LNAME()
	-------
	LName:
		Number Name
*/
const (char) [] LName(size_t max_digits)
	{
	const (char) [] name;

	if (!isDigit(peek_next()))
		return name;

	foreach (current; 0 .. number(max_digits))
		{
		if (peek_next() == 0)
			throw new Exception("badly formed LName");
		name ~= get_next();
		}

	return name;
	}
/*
	HEXDIGITS()
	-----------
	HexDigits:
		HexDigit
		HexDigit HexDigits

	HexDigit:
		Digit
		A
		B
		C
		D
		E
		F
*/
const (char) [] HexDigits()
{
const (char) [] answer;

while (true)
	switch (peek_next())
		{
		case '0': .. case'9':
		case 'A': .. case'Z':
			answer ~= get_next();
		default:
			return answer;
		}
}

/*
	EXPONENT()
	----------
	NOTE:  An exponent must start with a 'P'.
	Exponent:
		N Number
		Number
*/
const (char) [] Exponent()
	{
	if (peek_next() != 'P')
		return "";
	get_next();
	return to!string(number());
	}

/*
	HEXFLOAT()
	HexFloat:
		NAN
		INF
		NINF
		N HexDigits P Exponent
		HexDigits P Exponent

	Exponent:
		N Number
		Number
*/
const (char) [] HexFloat()
	{
	const (char) [] answer;

	switch (peek_next())
		{
		case 'I':			// INF
			if (peek_next(1) == 'N' && peek_next(2) == 'F')
				{
				get_next();
				get_next();
				get_next();
				return "real.infinity";
				}
			return answer;
		case 'N':
			switch (peek_next(1))
				{
				case 'A':			// NAN
					if (peek_next(2) == 'N')
						{
						get_next();
						get_next();
						get_next();
						return "real.nan";
						}
					/*
						NAB: Negative 0xAB! This is a nasty case.
					*/
					auto hex = HexDigits();
					answer ~= '-' ~ "0x" ~ hex[0] ~ "." ~ hex[1..$] ~ Exponent();
					return answer;
				case 'I':			// NINF
					if (peek_next(2) == 'N' && peek_next(3) == 'F')
						{
						get_next();
						get_next();
						get_next();
						get_next();
						return "-real.infinity";
						}
				case '0': .. case'9':		// a negative number (That's what the N stands for)
				case 'B': .. case'F':
					auto hex = HexDigits();
					answer ~= '-' ~ "0x" ~ hex[0] ~ "." ~ hex[1..$] ~ Exponent();
					return answer;
				default:
					return answer;
				}
		case '0': .. case'9':		// a negative number (That's what the N stands for)
		case 'A': .. case'F':
			auto hex = HexDigits();
			answer ~= '-' ~ "0x" ~ hex[0] ~ "." ~ hex[1..$] ~ Exponent();
			return answer;
		default:
			return answer;
		}
	}

/*
	NYBBLE()
	--------
*/
size_t nybble()
	{
	auto character = get_next();
	return character >= '0' && character <= '9' ? character - '0' :
		character >='A' && character <='F' ? character - 'A' + 10 :
		character >='a' && character <='f' ? character - 'a' + 10 :
	0;
	}

/*
	BYTE_HEX_NUMBER()
	-----------------
*/
size_t byte_hex_number()
	{
	switch (peek_next())
		{
		case '0': .. case'9':		// a negative number (That's what the N stands for)
		case 'A': .. case'F':
		case 'a': .. case'f':
			switch (peek_next(1))
				{
				case '0': .. case'9':		// a negative number (That's what the N stands for)
				case 'A': .. case'F':
				case 'a': .. case'f':
					size_t n1 = nybble();
					size_t n2 = nybble();
					return (n1 << 4) | n2;
				default:
					return 0;
				}
		default:
			return 0;
		}
	}

/*
	TO_HEX()
	--------
*/
const (char) [] to_hex(ulong number, size_t digits)
	{
	auto writer = appender!string();
	formattedWrite(writer, "%0*x", digits, number);
	return writer.data;
	}

/*
	FORMAT_ANSWER()
	---------------
*/
const (char) [] format_value(char sign, ulong value, char as)
	{
	switch(as)
		{
		case 'a', 'u', 'w': // (char, wchar, dchar)
			switch(value)
				{
				case '\'':
					return "'\\''";
				case '\\':
					return "'\\\\'";
				case '\a':
					return "'\\a'";
				case '\b':
					return "'\\b'" ;
				case '\f':
					return "'\\f'";
				case '\n':
					return "'\\n'";
				case '\r':
					return "'\\r'";
				case '\t':
					return "'\\t'";
				case '\v':
					return "'\\v'";
				default:
					switch(as)
						{
						case 'a':
							if(value >= ' ' && value < '~')
								return "'" ~ cast(char)value ~ "'";
							return "\\x" ~ to_hex(value, 2);
						case 'u':
							return "'\\u" ~ to_hex(value, 4) ~ "'";
						case 'w':
							return "'\\U" ~ to_hex(value, 8) ~ "'";
						default:
							return to!string(value);
						}
				}
		case 'b':			// bool
			return value ? "true" : "false";
		case 'h', 't', 'k': // ubyte, ushort, uint
			return (sign ? "-" : "") ~ to!string(value) ~ "u";
		case 'l':			// long
			return (sign ? "-" : "") ~ to!string(value) ~ "L";
		case 'm':			// ulong
			return to!string(value) ~ "uL";
		default:
			return to!string(value);
		}
	}
/*
	VALUE()
	-------
	Value:
		n
		Number
		i Number
		N Number
		e HexFloat
		c HexFloat c HexFloat
		CharWidth Number _ HexDigits
		A Number Value...
		S Number Value...

	CharWidth:
		a
		w
		d
	*/
const (char) [] Value(const (char) [] name=null, char parent_type = '\0')
	{
	const (char) [] answer;
	size_t length;

	switch (peek_next())
		{
		case 'n':
			get_next();
			return "null";
		case 'i':
			get_next();
			return format_value('+', number(), parent_type);
		case 'N':
			get_next();							//negative number
			return format_value('-', number(), parent_type);
		case '0': .. case'9':
			return format_value('+', number(), parent_type);
		case 'e':
			get_next();
			return HexFloat();
		case 'c':
			get_next();
			return HexFloat() ~ "+" ~ HexFloat() ~ "i";
		case 'S':
			get_next();
			length = number();
			foreach (num; 0..length)
				answer ~= (num == 0 ? "" : ", ") ~ Value();
			return name ~ "(" ~ answer ~ ")";
		case 'A':
			get_next();
			length = number();

			if (parent_type == 'H')
				foreach (num; 0..length)
					answer ~= (num == 0 ? "" : ", ") ~ Value() ~ ":" ~ Value();
			else
				foreach (num; 0..length)
					answer ~= (num == 0 ? "" : ", ") ~ Value();
			return "[" ~ answer ~ "]";
		case 'a':		// CharWidth
		case 'w':
		case 'd':
			auto sub_type = get_next();
			length = number();
			get_next();				// should be an '_'
			answer ~= '"';
			foreach (num; 0..length)
				{
				auto got = byte_hex_number();
				if (got < ' ' || got > '~')
					answer ~= "\\x" ~ to_hex(got, 2);
				else
					answer ~= to!char(got);
				}
			answer ~= '"';
			if (sub_type != 'a')
				answer ~= sub_type;
			return answer;
		default:
			return "";
		}
	}

/*
	TEMPLATEARGS()
	--------------
	TemplateArgs:
		TemplateArg
		TemplateArg TemplateArgs

	TemplateArg:
		TemplateArgX
		H TemplateArgX

	TemplateArgX:
		T Type
		V Type Value
		S LName
*/
const (char) [] TemplateArgs()
	{
	size_t count = 0;
	const (char) [] answer;

	while (true)
		{
		switch (peek_next())
			{
			case 'H':
				get_next();
				break;
			case 'T':
				get_next();
				if (count++ != 0)
					answer ~= ", ";
				answer ~= Type("");
				break;
			case 'V':
				get_next();
				if (count++ != 0)
					answer ~= ", ";
				char t = peek_next();		// this is a thorny case where t == H means an associateive array.  the example in the unittest is HiiA2i1i2i3i4
				auto of = Type("");
				auto val = Value(of, t);
				answer ~= val;
				break;
			case 'S':
				get_next();
				if (count++ != 0)
					answer ~= ", ";
				answer ~= QualifiedName();
				break;
			case 'Z':
				get_next();
				return answer;
			default:
				return answer;
			}
		}
	return answer;
	}

/*
	QUALIFIEDNAME()
	---------------
	QualifiedName:
		SymbolName
		SymbolName QualifiedName

	SymbolName:
		LName
		TemplateInstanceName
*/
const (char) [] QualifiedName()
	{
	const (char) [] name;
	const (char) [] part;
	size_t parts = 0;
	size_t digits = 0;

	/*
		See D language Issue 3043 in Bugzilla (https://issues.dlang.org/show_bug.cgi?id=3043) which says:
	
			For example, this code
			--------------------
			module test;
			struct Temp(alias a) {}
			template sym() {}
			pragma(msg, Temp!(sym).mangleof);
			--------------------
			prints "4test20__T4TempS94test3symZ4Temp".
			Here sym is mangled to "S94test3sym"; the Number is "9" and the Name is "4test3sym". But a demangler will recognize the Number and the Name as "94" and "test3sym", respectively.
	*/
	while (isDigit(peek_next()))
		{
		auto original_source = source;
		auto original_end = end;

		digits = 0;
		while (isDigit(peek_next(digits)))
			digits++;

		while (true)
			{
			try
				{
				part = LName(digits);
				if (part.length == 0)
					break;
				if (parts++ != 0)
					name ~= '.';

				if (part.length >= 3 && part[0..3] == "__T")
					name ~= demangle_entry(part[3..$], true);			// template so do a recursive call
				else if (part.length > 2 && part[0..2] == "_D")			// oh really?  This actually happens?
					name ~= demangle_entry(part[2..$], false);
				else if (isDigit(part[0]))								// Issue 3043
					name ~= demangle_entry(part, false);
				else
					name ~= part;
				break;
				}
			catch
				{
				if (digits <= 1)
					throw new Exception("badly formed LName");
				source = original_source;
				end = original_end;
				name = "";
				parts = 0;
				part = "";
				digits--;
				}
			}
		}
	return name;
	}

/*
	TYPEMODIFIERS()
	---------------
	TypeModifiers
		Const
		Wild
		Wild Const
		Shared
		Shared Const
		Shared Wild
		Shared Wild Const
		Immutable

	Shared:
		O

	Const:
		x

	Immutable:
		y

	Wild:
		Ng
*/
const (char) [] TypeModifiers()
	{
	const (char) [] modifiers;

	size_t count = 0;
	do
		{
		switch (peek_next())
			{
			case 'x':
				modifiers ~= (count != 0 ? " " : "") ~ "const";
				break;
			case 'N':			// "wild"
				if (peek_next(1) == 'g')
					{
					modifiers ~= (count != 0 ? " " : "") ~ "inout";
					get_next();
					}
				else
					return modifiers;
				break;
			case 'O':
				modifiers ~= (count != 0 ? " " : "") ~ "shared";
				break;
			case 'y':
				modifiers ~= (count != 0 ? " " : "") ~ "immutable";
				break;
			default:
				return modifiers;
			}
		get_next();
		count++;
		}
	while (true);

	return modifiers;
	}

/*
	CALLCONVENTION()
	----------------
	CallConvention:
		 F       // D
		 U       // C
		 W       // Windows
		 V       // Pascal
		 R       // C++
*/
const (char) [] CallConvention()
	{
	switch (peek_next())
		{
		case 'F':
			get_next();
			return "";
		case 'U':
			get_next();
			return "extern (C) ";
		case 'W':
			get_next();
			return "extern (Windows) ";
/*
			case 'V':
			get_next();
			return "extern (Pascal) ";
*/
		case 'R':
			get_next();
			return "extern (C++) ";
		default:
			return "";
		}
	}

/*
	IS_CALLCONVENTION()
	----------------
*/
bool is_CallConvention()
	{
	switch (peek_next())
		{
		case 'F':
		case 'U':
		case 'W':
//		case 'V':
		case 'R':
			return true;
		default:
			return false;
		}
	}


/*
	FUNCATTRS()
	-----------
	FuncAttrs:
		FuncAttr
		FuncAttr FuncAttrs

	FuncAttr:
		empty
		FuncAttrPure
		FuncAttrNothrow
		FuncAttrProperty
		FuncAttrRef
		FuncAttrTrusted
		FuncAttrSafe
		FuncAttrNogc

	FuncAttrPure:
		Na

	FuncAttrNothrow:
		Nb

	FuncAttrRef:
		Nc

	FuncAttrProperty:
		Nd

	FuncAttrTrusted:
		Ne

	FuncAttrSafe:
		Nf

	FuncAttrNogc:
		Ni
*/
const (char) [] FuncAttrs()
	{
	const (char) [] answer;

	while (true)
		if (peek_next() == 'N')
			{
			switch (peek_next(1))
				{
				case 'a':
					answer ~= "pure ";
					break;
				case 'b':
					answer ~= "nothrow ";
					break;
				case 'c':
					answer ~= "ref ";
					break;
				case 'd':
					answer ~= "@property ";
					break;
				case 'e':
					answer ~= "@trusted ";
					break;
				case 'f':
					answer ~= "@safe ";
					break;
				case 'i':
					answer ~= "@nogc ";
					break;
				default:
					return answer;
				}
			get_next();
			get_next();
			}
		else
			return answer;

	return answer;
	}

/*
	PARAMETERS()
	------------
	Parameters:
		Parameter
		Parameter Parameters

	Parameter:
		Parameter2
		M Parameter2     // scope

	Parameter2:
		Type
		J Type     // out
		K Type     // ref
		L Type     // lazy

	ParamClose
		X     // variadic T t...) style
		Y     // variadic T t,...) style
		Z     // not variadic
*/
const (char) [] Parameters()
	{
	size_t count = 0;
	const (char) [] answer;

	while (true)
		{
		switch (peek_next())
			{
			case 'M':
				get_next();
				if (count++ != 0)
					answer ~= ", ";
				answer ~= "scope " ~ Parameters();
				break;
			case 'J':
				get_next();
				if (count++ != 0)
					answer ~= ", ";
				answer ~= "out " ~ Type("", true);
				break;
			case 'K':
				get_next();
				if (count++ != 0)
					answer ~= ", ";
				answer ~= "ref " ~ Type("", true);
				break;
			case 'L':
				get_next();
				if (count++ != 0)
					answer ~= ", ";
				answer ~= "lazy " ~ Type("", true);
				break;
			case 'X':
				get_next();
				count++;
				answer ~= "...";
				return answer;;
			case 'Y':
				get_next();
				answer ~= ", ...";
				return answer;
			case 'Z':
				get_next();
				return answer;
			case '0': .. case '9':
				answer ~= QualifiedName();
				break;
			case 0:
				return answer;
			default:
				if (count++ != 0)
					answer ~= ", ";
				answer ~= Type("", true);
				break;
			}
		}
	}

/*
	TYPEFUNCTION()
	--------------
	TypeFunction:
		CallConvention FuncAttrs Parameters ParamClose Type
*/
const (char) [] TypeFunction(const (char) [] name, bool is_deligate = false, bool in_struct = false)
	{
	auto convention = CallConvention();
	auto func_attrs = FuncAttrs();
	auto params = Parameters();
	auto the_type = Type("", true, true);

	if (!in_struct)
		return convention ~ func_attrs ~ the_type ~ (the_type.length == 0 ? "" : " ") ~ name ~ (is_deligate ? "delegate" : "") ~ "(" ~ params ~ ")";
	else
		return name ~ "(" ~ params ~ ")";
	}

/*
	TYPEX()
	-------
	TypeX:
		TypeArray
		TypeStaticArray
		TypeAssocArray
		TypePointer
		TypeFunction
		TypeIdent
		TypeClass
		TypeStruct
		TypeEnum
		TypeTypedef
		TypeDelegate
		TypeVoid
		TypeByte
		TypeUbyte
		TypeShort
		TypeUshort
		TypeInt
		TypeUint
		TypeLong
		TypeUlong
		TypeFloat
		TypeDouble
		TypeReal
		TypeIfloat
		TypeIdouble
		TypeIreal
		TypeCfloat
		TypeCdouble
		TypeCreal
		TypeBool
		TypeChar
		TypeWchar
		TypeDchar
		TypeNull
[*]		TypeTuple				/// THESE DON'T APPEAR TO EXIST SO THEY ARE UNIMPLEMENTED
		TypeVector
		Internal

	TypeArray:
		A Type

	TypeStaticArray:
		G Number Type

	TypeAssocArray:
		H Type Type

	TypePointer:
		P Type

	TypeVector:
		Nh Type

	TypeIdent:
		I QualifiedName

	TypeClass:
		C QualifiedName

	TypeStruct:
		S QualifiedName

	TypeEnum:
		E QualifiedName

	TypeTypedef:
		T QualifiedName

	TypeDelegate:
		D TypeModifiers TypeFunction
		D TypeFunction

	TypeVoid:
		v

	TypeByte:
		g

	TypeUbyte:
		h

	TypeShort:
		s

	TypeUshort:
		t

	TypeInt:
		i

	TypeUint:
		k

	TypeLong:
		l

	TypeUlong:
		m

	TypeFloat:
		f

	TypeDouble:
		d

	TypeReal:
		e

	TypeIfloat:
		o

	TypeIdouble:
		p

	TypeIreal:
		j

	TypeCfloat:
		q

	TypeCdouble:
		r

	TypeCreal:
		c

	TypeBool:
		b

	TypeChar:
		a

	TypeWchar:
		u

	TypeDchar:
		w

	TypeNull:
		n

	Internal:
		Z
*/
const (char) [] TypeX(const (char) [] name = "", bool allow_deligates = false, bool in_struct = false)
	{
	static immutable string[23] primitives =
		[
		"char", // a
		"bool", // b
		"creal", // c
		"double", // d
		"real", // e
		"float", // f
		"byte", // g
		"ubyte", // h
		"int", // i
		"ireal", // j
		"uint", // k
		"long", // l
		"ulong", // m
		"null", // n
		"ifloat", // o
		"idouble", // p
		"cfloat", // q
		"cdouble", // r
		"short", // s
		"ushort", // t
		"wchar", // u
		"void", // v
		"dchar", // w
		];

	const (char) [] type;

	switch (peek_next())
		{
		case 'a': .. case 'w':			// base types
			return primitives[get_next() - 'a'] ~ (name.length == 0 ? "" : (" " ~ name));
		case 'A':							// TypeArray
			get_next();
			return Type("") ~ "[]" ~ (name.length == 0 ? "" : (" " ~ name));
		case 'F':							// CallConvention
		case 'U':
		case 'W':
		case 'V':
		case 'R':
			get_next();
			return TypeFunction(name, false, in_struct);
		case 'G':						// TypeStaticArray:
			get_next();
			auto count = number();
			auto of = Type("");
			return of ~ "[" ~ to!string(count) ~ "]" ~ (name.length == 0 ? "" : (" " ~ name));
		case 'H':						// TypeAssocArray
			get_next();
			auto type1 = Type("");
			auto type2 = Type("");
			return type1 ~ "[" ~ type2 ~ "]" ~ (name.length == 0 ? "" : (" " ~ name));
		case 'P':						// TypePointer
			get_next();
			auto of = Type("");
			return of ~ "*" ~ (name.length == 0 ? "" : (" " ~ name));
		case 'N':
			switch (peek_next(1))
				{
				case 'h':				// TypeVector
					get_next();
					get_next();
					return "__vector(" ~ Type("") ~ ")" ~ (name.length == 0 ? "" : (" " ~ name));
				default:
					throw new Exception ("Uknown type N" ~ peek_next(1));
				}
		case 'I':						// TypeIdent
		case 'C':						// TypeClass
		case 'S':						// TypeStruct
		case 'E':						// TypeEnum
		case 'T':						// TypeTypedef
			get_next();
			const (char) [] answer = QualifiedName() ~ (name.length == 0 ? "" : (" " ~ name));
			if (is_CallConvention())
				answer = Type(answer, false, false, true);
			while (isDigit(peek_next()))
				answer ~= "." ~ QualifiedName();
			return answer;
		case 'D': 						// TypeDelegate
			get_next();
			auto modifiers = TypeModifiers();
			auto func = TypeFunction(name, allow_deligates);
			if (modifiers.length != 0)
				return modifiers ~ " " ~ func;
			else
				return func;
			break;
		case 'z':
			switch (peek_next(1))
				{
				case 'k':
					get_next();
					get_next();
					return "ucent";
				case 'i':
					get_next();
					get_next();
					return "cent";
				default:
					break;
				}
		case 'Z':			// Internal (documented to exist but it is undocumented what it is)
			return name;
		default:
			return name;
		}

	return type;
	}

/*
	TYPE()
	------
	Type:
		TypeModifiers TypeX
		TypeX
*/
const (char) [] Type(const (char) [] name, bool allow_deligates = false, bool is_function = false, bool in_struct = false)
	{
	const (char) [] answer;

	auto mod = TypeModifiers();

	if (mod.length != 0)
		if (is_function)
			answer ~= mod ~ " ";
		else
			answer ~= mod ~ "(";

	answer ~= TypeX(name, allow_deligates, in_struct);

	if (mod.length != 0)
		if (is_function)
			answer ~= "";
		else
			answer ~= ")";

	return answer;
	}

/*
	DECODE()
	--------
*/
const (char) [] decode(bool is_template = false)
	{
	int count = 0;
	const (char) [] name;

	name ~= QualifiedName();
	if (name.length == 0)
		return "";

	if (peek_next() == 'M')
		get_next();		// discard the 'M'

	if (is_template)
		name ~= "!(" ~ TemplateArgs();
	else
		name = Type(name, false, true);

	while (isDigit(peek_next()))
		{
		name ~= "." ~ QualifiedName();

		if (peek_next() == 'M')
			get_next();		// discard the 'M'

		name = Type(name);
		}

	if (is_template)
		name ~= ")";

	return name;
	}

/*
	DEMANGLE_ENTRY()
	----------------
	recursive entry into the demangleing code.
*/
const (char)[] demangle_entry(const(char)[] mangled, bool is_template = false)
	{
	const (char) [] answer;
	auto stack_source = source;
	auto stack_end = end;

	source = mangled.ptr;
	end = source + mangled.length;
	answer = decode(is_template);
	source = stack_source;
	end = stack_end;

	return answer;
	}


/*
	DEMANGLE()
	----------
	_D QualifiedName Type
	_D QualifiedName M Type
*/
const (char)[] demangle(const(char)[] mangled, char[] destination = null)
	{
	if (mangled.length < 3)
		return mangled;
	if (mangled[0..2] != "_D")
		return mangled;

	/*
		Yup, main in not mangled in D so we have a special case checking for it here.
	*/
	if (mangled == "_Dmain")
		return "int main(string[] args)";

	/*
		We don't want any infinate loops so prevent them here.
	*/
	peek_operations = 0;
	max_allowable_peek_operations = mangled.length * 10;

	try
		{
		auto got = demangle_entry(mangled[2..$]);
		return got.length == 0 ? mangled : got;
		}
	catch
		return mangled;
	}

/*
	UNITTEST
	--------
*/
unittest
	{
	foreach(current, name; table)
		{
		if (current >= 0)
			{
			auto got = demangle(name[0]);
			if (got != name[1])
				{
				writeln("     test:", current, " FAILED");
				writeln("   Source:", name[0]);
				writeln("nexpected:", name[1]);
				writeln("      got:", got, "\n");
				return;
				}
			}
		}

	writeln("Passed demangle() unittest.");
	}

immutable string[2][] table =
	[
		/*
			These are nasty cases that came from the D bugzilla archive
		*/
		["_D4test20__T4TempS94test3symZ4Temp", "test.Temp!(test.sym).Temp"],																						// CORRECT: contains recursive length (see the 94)
		["_D1a3funPFZv", "void ()* a.fun"],																																	// UNKNOWN: pointer to function
		["_D8demangle32__T4testTS8demangle3fooVnnZ3barZ3bazFZv", "void demangle.test!(demangle.foo, null.bar).baz()"],								// WRONG:   ambiguity as 'V' is both the Pascal call convention and template parameter marker (Issue 14591)
		["_D8demangle27__T4testTS8demangle3fooVnnZ3bar3bazFZv_D1a5Class1cMxFZv", "void demangle.test!(demangle.foo, null).bar.baz()"],		// CORRECT: ambiguity as 'V' is both the Pascal call convention and template parameter marker (Issue 14591)
		["_D1a5Class1wMNgFZv", "inout void a.Class.w()"],																												// CORRECT: inout return value
		["_D16TypeInfo_HAyayAa6__initZ", "TypeInfo_HAyayAa.__init"],																								// CORRECT: the mangler appears to be incorrect in this case (Issue 11586)
		["_D4util13demangle_funs1A18さいごの果実MFiZv", "void util.demangle_funs.A.さいごの果実(int)"],														// CORRECT: unicode in mangled name (Issue 10393)
		["_D8demangle21__T2fnVHiiA2i1i2i3i4Z2fnFZv", "void demangle.fn!([1:2, 3:4]).fn()"],																	// CORRECT: associative array (Issue 6526)
		["_D8demangle1S2fnMFZv", "void demangle.S.fn()"],																												// CORRECT: function type modifiers
		["_D8demangle1S2fnMxFZv", "const void demangle.S.fn()"],																										// CORRECT: function type modifiers
		["_D8demangle1S2fnMyFZv", "immutable void demangle.S.fn()"],																								// CORRECT: function type modifiers
		["_D8demangle1S2fnMNgFZv", "inout void demangle.S.fn()"],																									// CORRECT: function type modifiers
		["_D8demangle1S2fnMOFZv", "shared void demangle.S.fn()"],																									// CORRECT: function type modifiers
		["_D8demangle1S2fnMOxFZv", "shared const void demangle.S.fn()"],																							// CORRECT: function type modifiers
		["_D8demangle1S2fnMONgFZv", "shared inout void demangle.S.fn()"],																							// CORRECT: function type modifiers
		["_D8demangle1S2fnMONgxFZv", "shared inout const void demangle.S.fn()"],																				// CORRECT: function type modifiers
		["_D8serenity9persister6Sqlite7__arrayZ", "serenity.persister.Sqlite.__array"],																		// CORRECT: non-functions
		["_D10TypeInfo_C6__vtblZ", "TypeInfo_C.__vtbl"],																												// CORRECT: C++ vtbl (virtual function tables)
		["_D2rt3aaA12__ModuleInfoZ", "rt.aaA.__ModuleInfo"],																											// CORRECT: Module

		// CORRECT: dots in return types
		["_D3dmd6Parser6Parser15parsePrimaryExpMFZC3dmd10Expression10Expression", "dmd.Expression.Expression dmd.Parser.Parser.parsePrimaryExp()"],

		// CORRECT: function local symbols (Issue 6045)
		["_D8serenity9persister6Sqlite70__T15SqlitePersisterTS8serenity9persister6Sqlite11__unittest6FZv4TestZ15SqlitePersister12__T7opIndexZ7opIndexMFmZS8serenity9persister6Sqlite11__unittest6FZv4Test", "serenity.persister.Sqlite.__unittest6().Test serenity.persister.Sqlite.SqlitePersister!(serenity.persister.Sqlite.__unittest6().Test).SqlitePersister.opIndex!().opIndex(ulong)"],

		// UNKNOWN:
		["_D3std11parallelism8TaskPool28__T6reduceVAyaa5_61202b2062Z130__T6reduceTS4test4mainFZv39__T3mapS28_D4test4mainFZv7getTermMFiZeZ49__T3mapTS3std5range15__T4iotaTyiTyiZ4iota6ResultZ3mapM6ResultZ6reduceMFS4test4mainFZv39__T3mapS28_D4test4mainFZv7getTermMFiZeZ49__T3mapTS3std5range15__T4iotaTyiTyiZ4iota6ResultZ3mapMFS3std5range15__T4iotaTyiTyiZ4iotaFyiyiZS3std5range15__T4iotaTyiTyiZ4iota6Result6ResultZS4test4mainFZv39__T3mapS28_D4test4mainFZv7getTermMFiZeZ49__T3mapTS3std5range15__T4iotaTyiTyiZ4iota6ResultZ3mapM6Result6ResultZe", "std.parallelism.TaskPool.reduce!(\"a + b\").reduce!(test.main().map!(real void test.main().getTerm(int)).map!(std.range.iota!(immutable(int), immutable(int)).iota.Result).map).reduce(test.main().map!(real void test.main().getTerm(int)).map!(std.range.iota!(immutable(int), immutable(int)).iota.Result).map, scope test.main().map!(real void test.main().getTerm(int)).map!(std.range.iota!(immutable(int), immutable(int)).iota.Result).map (std.range.iota!(immutable(int), immutable(int)).iota(immutable(int), immutable(int))), scope Result.Result, real)"],

		// WRONG: ambiguity in token length (see the 213).  Issue 14576
		["_D3std11parallelism8TaskPool28__T6reduceVAyaa5_61202b2062Z130__T6reduceTS3std9algorithm91__T9MapResultS27_D4test4mainFZ7getTermMFiZeTS3std5range15__T4iotaTyiTyiZ4iotaFyiyiZ6ResultZ9MapResultZ6reduceMFS3std9algorithm91__T9MapResultS27_D4test4mainFZ7getTermMFiZeTS3std5range15__T4iotaTyiTyiZ4iotaFyiyiZ6ResultZ9MapResultZ7useTaskMFNbNiNfKS3std11parallelism281__T4TaskS213std11parallelism3runTDFS3std9algorithm91__T9MapResultS27_D4test4mainFZ7getTermMFiZeTS3std5range15__T4iotaTyiTyiZ4iotaFyiyiZ6ResultZ9MapResultmmZeTS3std9algorithm91__T9MapResultS27_D4test4mainFZ7getTermMFiZeTS3std5range15__T4iotaTyiTyiZ4iotaFyiyiZ6ResultZ9MapResultTmTmZ4TaskZv", "nothrow @nogc @safe void std.parallelism.TaskPool.reduce!(\"a + b\").reduce!(std.algorithm.MapResult!(real test.main().getTerm(int), std.range.iota!(immutable(int), immutable(int)).iota(immutable(int), immutable(int)).Result).MapResult).reduce(std.algorithm.MapResult!(real test.main().getTerm(int), std.range.iota!(immutable(int), immutable(int)).iota(immutable(int), immutable(int)).Result).MapResult).useTask(ref std.parallelism.Task!(std11parallelism3runTDFS3std9algorithm91__T9MapResultS27_D4test4mainFZ7getTermMFiZeTS3std5range15__T4iotaTyiTyiZ4iotaFyiyiZ6ResultZ9MapResultmmZeTS3std9algorithm91__T9MapResultS27_D4test4mainFZ7getTermMFiZeTS3std5).Task)"],

		/*
			This is an attempt to get the mangler to generate a tuple token (which it does not)
		*/
		["_D4test6point1S3std8typecons32__T5TupleTiVAyaa1_78TiVAyaa1_79Z5Tuple", "std.typecons.Tuple!(int, \"x\", int, \"y\").Tuple test.point1"],

		/*
			These came from profiling this app.  There are three that don't agree with the D runtime demangle (coz it failes).  These are assumed to
			be correct here because the output here is believable.
		*/
		["_D11binary_tree118__T11binary_treeTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsZ11binary_tree22__T12find_and_addVbi1Z12find_and_addMFNbNcNiNfKC11binary_tree118__T11binary_treeTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsZ11binary_tree4nodeKxAaZS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postings", "nothrow ref @nogc @safe indexing_postings.indexing_postings!(7).indexing_postings binary_tree.binary_tree!(char[], indexing_postings.indexing_postings!(7).indexing_postings).binary_tree.find_and_add!(true).find_and_add(ref binary_tree.binary_tree!(char[], indexing_postings.indexing_postings!(7).indexing_postings).binary_tree.node, ref const(char[]))"],
		["_D11binary_tree118__T11binary_treeTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsZ11binary_tree4node6__ctorMFNbNiKxAaC9allocator9allocatorZC11binary_tree118__T11binary_treeTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsZ11binary_tree4node", "nothrow @nogc binary_tree.binary_tree!(char[], indexing_postings.indexing_postings!(7).indexing_postings).binary_tree.node binary_tree.binary_tree!(char[], indexing_postings.indexing_postings!(7).indexing_postings).binary_tree.node.__ctor(ref const(char[]), allocator.allocator)"],
		["_D11binary_tree118__T11binary_treeTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsZ11binary_tree6__ctorMFNbNiNfC9allocator9allocatorZC11binary_tree118__T11binary_treeTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsZ11binary_tree", "nothrow @nogc @safe binary_tree.binary_tree!(char[], indexing_postings.indexing_postings!(7).indexing_postings).binary_tree binary_tree.binary_tree!(char[], indexing_postings.indexing_postings!(7).indexing_postings).binary_tree.__ctor(allocator.allocator)"],
		["_D11binary_tree118__T11binary_treeTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsZ11binary_tree7opIndexMFNcKxAaZS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postings", "ref indexing_postings.indexing_postings!(7).indexing_postings binary_tree.binary_tree!(char[], indexing_postings.indexing_postings!(7).indexing_postings).binary_tree.opIndex(ref const(char[]))"],
		["_D11hash_random26__T11hash_randomVii24TAxaZ11hash_randomFNaNbNiNfxAaZm", "pure nothrow @nogc @safe ulong hash_random.hash_random!(24, const(char)[]).hash_random(const(char[]))"],
		["_D13instream_file13instream_file4readMFAaZm", "ulong instream_file.instream_file.read(char[])"],
		["_D13instream_file13instream_file6__ctorMFAyaZC13instream_file13instream_file", "instream_file.instream_file instream_file.instream_file.__ctor(immutable(char)[])"],
		["_D15stream_growable23__T15stream_growableTgZ15stream_growable5piece6__ctorMFNbNcNiNfC9allocator9allocatormZS15stream_growable23__T15stream_growableTgZ15stream_growable5piece", "nothrow ref @nogc @safe stream_growable.stream_growable!(byte).stream_growable.piece stream_growable.stream_growable!(byte).stream_growable.piece.__ctor(allocator.allocator, ulong)"],
		["_D15stream_growable23__T15stream_growableTgZ15stream_growable6__ctorMFNbNiNfKC9allocator9allocatormdZC15stream_growable23__T15stream_growableTgZ15stream_growable", "nothrow @nogc @safe stream_growable.stream_growable!(byte).stream_growable stream_growable.stream_growable!(byte).stream_growable.__ctor(ref allocator.allocator, ulong, double)"],
		["_D15stream_growable23__T15stream_growableTgZ15stream_growable9space_forMFNiNemZPg", "@nogc @trusted byte* stream_growable.stream_growable!(byte).stream_growable.space_for(ulong)"],
		["_D15stream_growable23__T15stream_growableTtZ15stream_growable16replace_ultimateMFNaNbNiNftZv", "pure nothrow @nogc @safe void stream_growable.stream_growable!(ushort).stream_growable.replace_ultimate(ushort)"],
		["_D15stream_growable23__T15stream_growableTtZ15stream_growable5piece6__ctorMFNbNcNiNfC9allocator9allocatormZS15stream_growable23__T15stream_growableTtZ15stream_growable5piece", "nothrow ref @nogc @safe stream_growable.stream_growable!(ushort).stream_growable.piece stream_growable.stream_growable!(ushort).stream_growable.piece.__ctor(allocator.allocator, ulong)"],
		["_D15stream_growable23__T15stream_growableTtZ15stream_growable6__ctorMFNbNiNfKC9allocator9allocatormdZC15stream_growable23__T15stream_growableTtZ15stream_growable", "nothrow @nogc @safe stream_growable.stream_growable!(ushort).stream_growable stream_growable.stream_growable!(ushort).stream_growable.__ctor(ref allocator.allocator, ulong, double)"],
		["_D15stream_growable23__T15stream_growableTtZ15stream_growable8ultimateMFNaNbNiNfZt", "pure nothrow @nogc @safe ushort stream_growable.stream_growable!(ushort).stream_growable.ultimate()"],
		["_D15stream_growable23__T15stream_growableTtZ15stream_growable9space_forMFNiNemZPt", "@nogc @trusted ushort* stream_growable.stream_growable!(ushort).stream_growable.space_for(ulong)"],
		["_D17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postings3addMFNiNfmmZv", "@nogc @safe void indexing_postings.indexing_postings!(7).indexing_postings.add(ulong, ulong)"],
		["_D17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postings6__ctorMFNbNcNiNfC9allocator9allocatorZS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postings", "nothrow ref @nogc @safe indexing_postings.indexing_postings!(7).indexing_postings indexing_postings.indexing_postings!(7).indexing_postings.__ctor(allocator.allocator)"],
		["_D23directory_iterator_line23directory_iterator_line10set_sourceMFC8instream8instreamZv", "void directory_iterator_line.directory_iterator_line.set_source(instream.instream)"],
		["_D23directory_iterator_line23directory_iterator_line8get_fileMFZAa", "char[] directory_iterator_line.directory_iterator_line.get_file()"],
		["_D28compress_variable_byte_atire20__T12bytes_neededTmZ12bytes_neededFNaNbNiNfmZm", "pure nothrow @nogc @safe ulong compress_variable_byte_atire.bytes_needed!(ulong).bytes_needed(ulong)"],
		["_D28compress_variable_byte_atire21__T13compress_intoTmZ13compress_intoFNaNbNiNePgmmZm", "pure nothrow @nogc @trusted ulong compress_variable_byte_atire.compress_into!(ulong).compress_into(byte*, ulong, ulong)"],
		["_D3std4conv55__T11toTextRangeTmTS3std5stdio4File17LockingTextWriterZ11toTextRangeFNfmS3std5stdio4File17LockingTextWriterZv", "@safe void std.conv.toTextRange!(ulong, std.stdio.File.LockingTextWriter).toTextRange(ulong, std.stdio.File.LockingTextWriter)"],
		["_D3std4conv91__T18emplaceInitializerTS15stream_growable23__T15stream_growableTgZ15stream_growable5pieceZ18emplaceInitializerFNaNbNcNiNeKS15stream_growable23__T15stream_growableTgZ15stream_growable5pieceZS15stream_growable23__T15stream_growableTgZ15stream_growable5piece", "pure nothrow ref @nogc @trusted stream_growable.stream_growable!(byte).stream_growable.piece std.conv.emplaceInitializer!(stream_growable.stream_growable!(byte).stream_growable.piece).emplaceInitializer(ref stream_growable.stream_growable!(byte).stream_growable.piece)"],
		["_D3std4conv91__T18emplaceInitializerTS15stream_growable23__T15stream_growableTtZ15stream_growable5pieceZ18emplaceInitializerFNaNbNcNiNeKS15stream_growable23__T15stream_growableTtZ15stream_growable5pieceZS15stream_growable23__T15stream_growableTtZ15stream_growable5piece", "pure nothrow ref @nogc @trusted stream_growable.stream_growable!(ushort).stream_growable.piece std.conv.emplaceInitializer!(stream_growable.stream_growable!(ushort).stream_growable.piece).emplaceInitializer(ref stream_growable.stream_growable!(ushort).stream_growable.piece)"],
		["_D3std5array102__T5arrayTS3std9algorithm9iteration40__T8splitterVAyaa6_61203d3d2062TAyaTAyaZ8splitterFAyaAyaZ6ResultZ5arrayFNaNbNfS3std9algorithm9iteration40__T8splitterVAyaa6_61203d3d2062TAyaTAyaZ8splitterFAyaAyaZ6ResultZAAya", "pure nothrow @safe immutable(char)[][] std.array.array!(std.algorithm.iteration.splitter!(\"a == b\", immutable(char)[], immutable(char)[]).splitter(immutable(char)[], immutable(char)[]).Result).array(std.algorithm.iteration.splitter!(\"a == b\", immutable(char)[], immutable(char)[]).splitter(immutable(char)[], immutable(char)[]).Result)"],
		["_D3std5array18__T8AppenderTAAyaZ8Appender12__T3putTAyaZ3putMFAyaZ9__lambda2MFNaNbNiNeZAAya", "pure nothrow @nogc @trusted immutable(char)[][] std.array.Appender!(immutable(char)[][]).Appender.put!(immutable(char)[]).put(immutable(char)[]).__lambda2()"],
		["_D3std5array18__T8AppenderTAAyaZ8Appender12__T3putTAyaZ3putMFNaNbNfAyaZv", "pure nothrow @safe void std.array.Appender!(immutable(char)[][]).Appender.put!(immutable(char)[]).put(immutable(char)[])"],
		["_D3std5array18__T8AppenderTAAyaZ8Appender13ensureAddableMFNaNbNemZv", "pure nothrow @trusted void std.array.Appender!(immutable(char)[][]).Appender.ensureAddable(ulong)"],
		["_D3std5array18__T8AppenderTAAyaZ8Appender6__ctorMFNaNbNcNeAAyaZS3std5array18__T8AppenderTAAyaZ8Appender", "pure nothrow ref @trusted std.array.Appender!(immutable(char)[][]).Appender std.array.Appender!(immutable(char)[][]).Appender.__ctor(immutable(char)[][])"],
		["_D3std5array30__T19appenderNewCapacityVmi16Z19appenderNewCapacityFNaNbNiNfmmZm", "pure nothrow @nogc @safe ulong std.array.appenderNewCapacity!(16uL).appenderNewCapacity(ulong, ulong)"],
		["_D3std5stdio20__T7writelnTAyaTAyaZ7writelnFNfAyaAyaZv", "@safe void std.stdio.writeln!(immutable(char)[], immutable(char)[]).writeln(immutable(char)[], immutable(char)[])"],
		["_D3std5stdio24__T7writelnTAyaTmTAyaTmZ7writelnFNfAyamAyamZv", "@safe void std.stdio.writeln!(immutable(char)[], ulong, immutable(char)[], ulong).writeln(immutable(char)[], ulong, immutable(char)[], ulong)"],
		["_D3std5stdio4File17LockingTextWriter10__T3putTaZ3putMFNbNiNfaZv", "nothrow @nogc @safe void std.stdio.File.LockingTextWriter.put!(char).put(char)"],
		["_D3std5stdio4File17LockingTextWriter11__T3putTAaZ3putMFNfAaZv", "@safe void std.stdio.File.LockingTextWriter.put!(char[]).put(char[])"],
		["_D3std5stdio4File17LockingTextWriter12__T3putTAyaZ3putMFNfAyaZv", "@safe void std.stdio.File.LockingTextWriter.put!(immutable(char)[]).put(immutable(char)[])"],
		["_D3std5stdio4File20__T5writeTAyaTAyaTaZ5writeMFNfAyaAyaaZv", "@safe void std.stdio.File.write!(immutable(char)[], immutable(char)[], char).write(immutable(char)[], immutable(char)[], char)"],
		["_D3std5stdio4File24__T5writeTAyaTmTAyaTmTaZ5writeMFNfAyamAyamaZv", "@safe void std.stdio.File.write!(immutable(char)[], ulong, immutable(char)[], ulong, char).write(immutable(char)[], ulong, immutable(char)[], ulong, char)"],
		["_D3std6getopt16__T10getoptImplZ10getoptImplFNfKAAyaKS3std6getopt13configurationKS3std6getopt12GetoptResultZv", "@safe void std.getopt.getoptImpl!().getoptImpl(ref immutable(char)[][], ref std.getopt.configuration, ref std.getopt.GetoptResult)"],
		["_D3std6getopt21__T12handleOptionTPbZ12handleOptionFAyaPbKAAyaKS3std6getopt13configurationbZb", "bool std.getopt.handleOption!(bool*).handleOption(immutable(char)[], bool*, ref immutable(char)[][], ref std.getopt.configuration, bool)"],
		["_D3std6getopt23__T12handleOptionTPAyaZ12handleOptionFAyaPAyaKAAyaKS3std6getopt13configurationbZb", "bool std.getopt.handleOption!(immutable(char)[]*).handleOption(immutable(char)[], immutable(char)[]*, ref immutable(char)[][], ref std.getopt.configuration, bool)"],
		["_D3std6getopt40__T10getoptImplTAyaTAyaTPAyaTAyaTAyaTPbZ10getoptImplFKAAyaKS3std6getopt13configurationKS3std6getopt12GetoptResultAyaAyaPAyaAyaAyaPbZv", "void std.getopt.getoptImpl!(immutable(char)[], immutable(char)[], immutable(char)[]*, immutable(char)[], immutable(char)[], bool*).getoptImpl(ref immutable(char)[][], ref std.getopt.configuration, ref std.getopt.GetoptResult, immutable(char)[], immutable(char)[], immutable(char)[]*, immutable(char)[], immutable(char)[], bool*)"],
		["_D3std6getopt55__T6getoptTE3std6getopt6configTAyaTAyaTPAyaTAyaTAyaTPbZ6getoptFKAAyaE3std6getopt6configAyaAyaPAyaAyaAyaPbZS3std6getopt12GetoptResult", "std.getopt.GetoptResult std.getopt.getopt!(std.getopt.config, immutable(char)[], immutable(char)[], immutable(char)[]*, immutable(char)[], immutable(char)[], bool*).getopt(ref immutable(char)[][], std.getopt.config, immutable(char)[], immutable(char)[], immutable(char)[]*, immutable(char)[], immutable(char)[], bool*)"],
		["_D3std9algorithm10comparison32__T3cmpVAyaa5_61203c2062TAaTAxaZ3cmpFAaAxaZ9__lambda6MFNaNbNiNeZi", "pure nothrow @nogc @trusted int std.algorithm.comparison.cmp!(\"a < b\", char[], const(char)[]).cmp(char[], const(char)[]).__lambda6()"],
		["_D3std9algorithm10comparison32__T3cmpVAyaa5_61203c2062TAaTAxaZ3cmpFNaNbNiNfAaAxaZi", "pure nothrow @nogc @safe int std.algorithm.comparison.cmp!(\"a < b\", char[], const(char)[]).cmp(char[], const(char)[])"],
		["_D3std9algorithm9iteration40__T8splitterVAyaa6_61203d3d2062TAyaTAyaZ8splitterFAyaAyaZ6Result17ensureFrontLengthMFNaNbNiNfZv", "pure nothrow @nogc @safe void std.algorithm.iteration.splitter!(\"a == b\", immutable(char)[], immutable(char)[]).splitter(immutable(char)[], immutable(char)[]).Result.ensureFrontLength()"],
		["_D3std9algorithm9iteration40__T8splitterVAyaa6_61203d3d2062TAyaTAyaZ8splitterFAyaAyaZ6Result6__ctorMFNaNbNcNiNfAyaAyaZS3std9algorithm9iteration40__T8splitterVAyaa6_61203d3d2062TAyaTAyaZ8splitterFAyaAyaZ6Result", "pure nothrow ref @nogc @safe std.algorithm.iteration.splitter!(\"a == b\", immutable(char)[], immutable(char)[]).splitter(immutable(char)[], immutable(char)[]).Result std.algorithm.iteration.splitter!(\"a == b\", immutable(char)[], immutable(char)[]).splitter(immutable(char)[], immutable(char)[]).Result.__ctor(immutable(char)[], immutable(char)[])"],
		["_D3std9algorithm9iteration40__T8splitterVAyaa6_61203d3d2062TAyaTAyaZ8splitterFAyaAyaZ6Result8popFrontMFNaNbNiNfZv", "pure nothrow @nogc @safe void std.algorithm.iteration.splitter!(\"a == b\", immutable(char)[], immutable(char)[]).splitter(immutable(char)[], immutable(char)[]).Result.popFront()"],
		["_D3std9algorithm9iteration40__T8splitterVAyaa6_61203d3d2062TAyaTAyaZ8splitterFNaNbNiNfAyaAyaZS3std9algorithm9iteration40__T8splitterVAyaa6_61203d3d2062TAyaTAyaZ8splitterFAyaAyaZ6Result", "pure nothrow @nogc @safe std.algorithm.iteration.splitter!(\"a == b\", immutable(char)[], immutable(char)[]).splitter(immutable(char)[], immutable(char)[]).Result std.algorithm.iteration.splitter!(\"a == b\", immutable(char)[], immutable(char)[]).splitter(immutable(char)[], immutable(char)[])"],
		["_D3std9algorithm9searching34__T4findVAyaa6_61203d3d2062TAhTAhZ4findFNaNbNiNfAhAhZAh", "pure nothrow @nogc @safe ubyte[] std.algorithm.searching.find!(\"a == b\", ubyte[], ubyte[]).find(ubyte[], ubyte[])"],
		["_D3std9algorithm9searching36__T4findVAyaa6_61203d3d2062TAyaTAyaZ4findFNaNbNiNfAyaAyaZAya", "pure nothrow @nogc @safe immutable(char)[] std.algorithm.searching.find!(\"a == b\", immutable(char)[], immutable(char)[]).find(immutable(char)[], immutable(char)[])"],
		["_D3std9algorithm9searching41__T10startsWithVAyaa6_61203d3d2062TAhTAhZ10startsWithFNaNbNiNfAhAhZb", "pure nothrow @nogc @safe bool std.algorithm.searching.startsWith!(\"a == b\", ubyte[], ubyte[]).startsWith(ubyte[], ubyte[])"],
		["_D3std9exception27__T7enforceHTC9ExceptionTbZ7enforceFNaNfbLAxaAyamZb", "pure @safe bool std.exception.enforce!(Exception, bool).enforce(bool, lazy const(char)[], immutable(char)[], ulong)"],
		["_D3std9exception27__T7enforceHTC9ExceptionTmZ7enforceFNaNfmLAxaAyamZm", "pure @safe ulong std.exception.enforce!(Exception, ulong).enforce(ulong, lazy const(char)[], immutable(char)[], ulong)"],
		["_D3str9toStringzFAaAyaZb", "bool str.toStringz(char[], immutable(char)[])"],
		["_D4file4file4openMFAyaAyaZb", "bool file.file.open(immutable(char)[], immutable(char)[])"],
		["_D4file4file4readMFAvZAv", "void[] file.file.read(void[])"],
		["_D4hash115__T4hashTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsVii24Z4hash6__ctorMFNbNiC9allocator9allocatorZC4hash115__T4hashTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsVii24Z4hash", "nothrow @nogc hash.hash!(char[], indexing_postings.indexing_postings!(7).indexing_postings, 24).hash hash.hash!(char[], indexing_postings.indexing_postings!(7).indexing_postings, 24).hash.__ctor(allocator.allocator)"],
		["_D4hash115__T4hashTAaTS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postingsVii24Z4hash7opIndexMFNcKxAaZS17indexing_postings56__T17indexing_postingsVE17indexing_postings9attributei7Z17indexing_postings", "ref indexing_postings.indexing_postings!(7).indexing_postings hash.hash!(char[], indexing_postings.indexing_postings!(7).indexing_postings, 24).hash.opIndex(ref const(char[]))"],
		["_D9allocator9allocator13system_mallocMFNbNiNemZPg", "nothrow @nogc @trusted byte* allocator.allocator.system_malloc(ulong)"],
		["_D9allocator9allocator6__ctorMFNaNbNiNfmZC9allocator9allocator", "pure nothrow @nogc @safe allocator.allocator allocator.allocator.__ctor(ulong)"],
		["_D9allocator9allocator6mallocMFNbNiNemZAg", "nothrow @nogc @trusted byte[] allocator.allocator.malloc(ulong)"],
		["_D9tokenizer9tokenizer10set_sourceMFAaZv", "void tokenizer.tokenizer.set_source(char[])"],
		["_D9tokenizer9tokenizer12__T5parseTmZ5parseMFNaNbNiNemZAa", "pure nothrow @nogc @trusted char[] tokenizer.tokenizer.parse!(ulong).parse(ulong)"],
		["_Dmain", "int main(string[] args)"],

		/*
			These came from the D runtime core.demangle.d code base. That code contains the following Copyright notice.  The "accompanying file LICENSE" is here: http://www.boost.org/LICENSE_1_0.txt
				Copyright: Copyright Sean Kelly 2010 - 2014.
				License: Distributed under the
					$(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
					(See accompanying file LICENSE)
				Authors:   Sean Kelly
				Source:    $(DRUNTIMESRC core/_demangle.d)
		*/
		["printf", "printf"],
		["_foo", "_foo"],
		["_D88", "_D88"],
		["_D4test3fooAa", "char[] test.foo"],
		["_D8demangle8demangleFAaZAa", "char[] demangle.demangle(char[])"],
		["_D6object6Object8opEqualsFC6ObjectZi", "int object.Object.opEquals(Object)"],
		["_D4test2dgDFiYd", "double test.dg(int, ...)"],
		//["_D4test58__T9factorialVde67666666666666860140VG5aa5_68656c6c6fVPvnZ9factorialf", ""],
		//["_D4test101__T9factorialVde67666666666666860140Vrc9a999999999999d9014000000000000000c00040VG5aa5_68656c6c6fVPvnZ9factorialf", ""],
		["_D4test34__T3barVG3uw3_616263VG3wd3_646566Z1xi", "int test.bar!(\"abc\"w, \"def\"d).x"],
		["_D8demangle4testFLC6ObjectLDFLiZiZi", "int demangle.test(lazy Object, lazy int delegate(lazy int))"],
		["_D8demangle4testFAiXi", "int demangle.test(int[]...)"],
		["_D8demangle4testFAiYi", "int demangle.test(int[], ...)"],
		["_D8demangle4testFLAiXi", "int demangle.test(lazy int[]...)"],
		["_D8demangle4testFLAiYi", "int demangle.test(lazy int[], ...)"],
		["_D6plugin8generateFiiZAya", "immutable(char)[] plugin.generate(int, int)"],
		["_D6plugin8generateFiiZAxa", "const(char)[] plugin.generate(int, int)"],
		["_D6plugin8generateFiiZAOa", "shared(char)[] plugin.generate(int, int)"],
		["_D8demangle3fnAFZ3fnBMFZv", "void demangle.fnA().fnB()"],
		["_D8demangle4mainFZ1S3fnCMFZv", "void demangle.main().S.fnC()"],
		["_D8demangle4mainFZ1S3fnDMFZv", "void demangle.main().S.fnD()"],
		["_D8demangle20__T2fnVAiA4i1i2i3i4Z2fnFZv", "void demangle.fn!([1, 2, 3, 4]).fn()"],
		["_D8demangle10__T2fnVi1Z2fnFZv", "void demangle.fn!(1).fn()"],
		["_D8demangle26__T2fnVS8demangle1SS2i1i2Z2fnFZv", "void demangle.fn!(demangle.S(1, 2)).fn()"],
		["_D8demangle13__T2fnVeeNANZ2fnFZv", "void demangle.fn!(real.nan).fn()"],
		["_D8demangle14__T2fnVeeNINFZ2fnFZv", "void demangle.fn!(-real.infinity).fn()"],
		["_D8demangle13__T2fnVeeINFZ2fnFZv", "void demangle.fn!(real.infinity).fn()"],
		["_D8demangle21__T2fnVHiiA2i1i2i3i4Z2fnFZv", "void demangle.fn!([1:2, 3:4]).fn()"],
		["_D8demangle2fnFNgiZNgi", "inout int demangle.fn(inout(int))"],
		["_D8demangle29__T2fnVa97Va9Va0Vu257Vw65537Z2fnFZv", "void demangle.fn!('a', '\\t', \\x00, '\\u0101', '\\U00010001').fn()"],
		["_D2gc11gctemplates56__T8mkBitmapTS3std5range13__T4iotaTiTiZ4iotaFiiZ6ResultZ8mkBitmapFNbNiNfPmmZv",
		"nothrow @nogc @safe void gc.gctemplates.mkBitmap!(std.range.iota!(int, int).iota(int, int).Result).mkBitmap(ulong*, ulong)"],
		["_D8serenity9persister6Sqlite69__T15SqlitePersisterTS8serenity9persister6Sqlite11__unittest6FZ4TestZ15SqlitePersister12__T7opIndexZ7opIndexMFmZS8serenity9persister6Sqlite11__unittest6FZ4Test",
		"serenity.persister.Sqlite.__unittest6().Test serenity.persister.Sqlite.SqlitePersister!(serenity.persister.Sqlite.__unittest6().Test).SqlitePersister.opIndex!().opIndex(ulong)"],
		["_D8bug100274mainFZ5localMFZi","int bug10027.main().local()"],
		["_D8demangle4testFNhG16gZv", "void demangle.test(__vector(byte[16]))"],
		["_D8demangle4testFNhG8sZv", "void demangle.test(__vector(short[8]))"],
		["_D8demangle4testFNhG4iZv", "void demangle.test(__vector(int[4]))"],
		["_D8demangle4testFNhG2lZv", "void demangle.test(__vector(long[2]))"],
		["_D8demangle4testFNhG4fZv", "void demangle.test(__vector(float[4]))"],
		["_D8demangle4testFNhG2dZv", "void demangle.test(__vector(double[2]))"],
		["_D8demangle4testFNhG4fNhG4fZv", "void demangle.test(__vector(float[4]), __vector(float[4]))"],
		["_D8bug1119234__T3fooS23_D8bug111924mainFZ3bariZ3fooMFZv","void bug11192.foo!(int bug11192.main().bar).foo()"],
		["_D13libd_demangle12__ModuleInfoZ", "libd_demangle.__ModuleInfo"],
		["_D15TypeInfo_Struct6__vtblZ", "TypeInfo_Struct.__vtbl"],
		["_D3std5stdio12__ModuleInfoZ", "std.stdio.__ModuleInfo"],
		["_D3std6traits15__T8DemangleTkZ8Demangle6__initZ", "std.traits.Demangle!(uint).Demangle.__init"],
		["_D3foo3Bar7__ClassZ", "foo.Bar.__Class"],
		["_D3foo3Bar6__vtblZ", "foo.Bar.__vtbl"],
		["_D3foo3Bar11__interfaceZ", "foo.Bar.__interface"],
		["_D3foo7__arrayZ", "foo.__array"],
		["_D8link657428__T3fooVE8link65746Methodi0Z3fooFZi", "int link6574.foo!(0).foo()"],
		["_D8link657429__T3fooHVE8link65746Methodi0Z3fooFZi", "int link6574.foo!(0).foo()"],
		["_D4test22__T4funcVAyaa3_610a62Z4funcFNaNbNiNfZAya", `pure nothrow @nogc @safe immutable(char)[] test.func!("a\x0ab").func()`],
		["_D3foo3barFzkZzi", "cent foo.bar(ucent)"],
	];
