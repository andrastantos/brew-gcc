// Copyright (C) 2003 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <new>
#include <locale>
#include <cstdlib>
#include <cstring>

int times_to_fail = 0;

void* allocate(std::size_t n)
{
  if (!times_to_fail--)
    return 0;

  void* ret = std::malloc(n ? n : 1);
  if (ret)
    std::memset(ret, 0xbc, n);
  return ret;
}

void deallocate(void* p)
{
  if (p)
    std::free(p);
}

void* operator new(std::size_t n) throw (std::bad_alloc)
{
  void* ret = allocate(n);
  if (!ret)
    throw std::bad_alloc();
  return ret;
}

void* operator new[](std::size_t n) throw (std::bad_alloc)
{
  void* ret = allocate(n);
  if (!ret)
    throw std::bad_alloc();
  return ret;
}

void operator delete(void* p) throw()
{
  deallocate(p);
}

void operator delete[](void* p) throw()
{
  deallocate(p);
}

void* operator new(std::size_t n, const std::nothrow_t&) throw()
{
  return allocate(n);
}

void* operator new[](std::size_t n, const std::nothrow_t&) throw()
{
  return allocate(n);
}

void operator delete(void* p, const std::nothrow_t&) throw()
{
  deallocate(p);
}

void operator delete[](void* p, const std::nothrow_t&) throw()
{
  deallocate(p);
}

// libstdc++/12352
void test01(int iters)
{
  using namespace std;
  bool test = true;

  for (int j = 0; j < iters; ++j)
    {
      for (int i = 0; i < 100; ++i)
	{
	  times_to_fail = i;
	  try
	    {
	      locale loc1("");
	      locale loc2(loc1, locale::classic(), locale::numeric);
	    }
	  catch (exception&)
	    {
	    }
	}
    }
}

int main(int argc, char* argv[])
{
  int iters = 1;
  if (argc > 1)
    iters = std::atoi(argv[1]);
  if (iters < 1)
    iters = 1;
  test01(iters);

  return 0;
}
