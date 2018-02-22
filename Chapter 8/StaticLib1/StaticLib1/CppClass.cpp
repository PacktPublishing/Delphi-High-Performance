#include "stdafx.h"
#include "CppClass.h"

CppClass::CppClass()
{
}

CppClass::~CppClass()
{
}

void CppClass::setData(int value)
{
	data = value;
}

int CppClass::getSquare()
{
	return data * data;
}
