//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2011-2021 Michael Truog <mjtruog at protonmail dot com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

#include <cstdlib>
#include <sstream>
#include <exception>
#include <boost/exception/all.hpp>
#undef assert
#include "assert.hpp"

namespace
{
    class assert_exception_msg : public std::exception
    {
        public:
            assert_exception_msg(std::string const & message) throw () :
                m_message(message)
            {
            }
            virtual ~assert_exception_msg() throw ()
            {
            }
            virtual char const * what() const throw ()
            {
                return m_message.c_str();
            }
        private:
            std::string m_message;
    };

    class assert_exception : public std::exception
    {
        public:
            assert_exception(std::string const & message) throw () :
                m_message(message)
            {
            }
            virtual ~assert_exception() throw ()
            {
            }
            virtual char const * what() const throw ()
            {
                return m_message.c_str();
            }
        private:
            std::string m_message;
    };
}

namespace boost
{
    void assertion_failed_msg(char const * expr,
                              char const * function,
                              char const * file,
                              char const * mm,
                              long line)
    {
        std::ostringstream stream;
        stream << "assert failure: " << expr << ": " << mm;
        throw (boost::enable_error_info(assert_exception_msg(stream.str())) <<
               boost::throw_function(function) <<
               boost::throw_file(file) <<
               boost::throw_line(line));
    }

    void assertion_failed(char const * expr,
                          char const * function,
                          char const * file,
                          long line)
    {
        std::ostringstream stream;
        stream << "assert failure: " << expr;
        throw (boost::enable_error_info(assert_exception(stream.str())) <<
               boost::throw_function(function) <<
               boost::throw_file(file) <<
               boost::throw_line(line));
    }
}

