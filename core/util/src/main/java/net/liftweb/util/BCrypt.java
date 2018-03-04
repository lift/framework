/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
// ---------------
// This is a derivative work of jBCrypt, distributed under BSD licence
// and copyrighted as follows:
//
// Copyright (c) 2006 Damien Miller <djm@mindrot.org>
//
// Permission to use, copy, modify, and distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

package net.liftweb.util;

import java.io.UnsupportedEncodingException;

import java.security.SecureRandom;

/**
 * This class is a passthrough to org.mindrot.jbcrypt.BCrypt, provided for
 * backwards compatibility as we kept a copy in the Lift codebase before it was
 * available for package dependencies. Prefer using org.mindrot.jbcrypt.BCrypt.
 */
@Deprecated
public class BCrypt {
    /**
     * @see org.mindrot.jbcrypt.BCrypt#hashpw(String,String)
     */
    @Deprecated
    public static String hashpw(String password, String salt) {
        return org.mindrot.jbcrypt.BCrypt.hashpw(password, salt);
    }

    /**
     * @see org.mindrot.jbcrypt.BCrypt#gensalt(int,SecureRandom)
     */
    @Deprecated
    public static String gensalt(int log_rounds, SecureRandom random) {
        return org.mindrot.jbcrypt.BCrypt.gensalt(log_rounds, random);
    }

    /**
     * @see org.mindrot.jbcrypt.BCrypt#gensalt(int)
     */
    @Deprecated
    public static String gensalt(int log_rounds) {
        return org.mindrot.jbcrypt.BCrypt.gensalt(log_rounds);
    }

    /**
     * @see org.mindrot.jbcrypt.BCrypt#gensalt()
     */
    @Deprecated
    public static String gensalt() {
        return org.mindrot.jbcrypt.BCrypt.gensalt();
    }

    /**
     * @see org.mindrot.jbcrypt.BCrypt#checkpw(String,String)
     */
    @Deprecated
    public static boolean checkpw(String plaintext, String hashed) {
        return org.mindrot.jbcrypt.BCrypt.checkpw(plaintext, hashed);
    }
}
