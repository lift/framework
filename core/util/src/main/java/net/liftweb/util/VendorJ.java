/*
 * Copyright 2011 Lift Committers and Contributors
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

package net.liftweb.util;

import net.liftweb.common.Func0;
import java.util.concurrent.Callable;

/**
 * The bridge from Java to Lift's Vendor stuff
 */
public final class VendorJ {
    private static VendorJBridge j = new VendorJBridge();
    
    /**
     * Create a Vendor from a Func0
     */
    public static<T> Vendor<T> vendor(Func0<T> f) {
	return j.vendor(f);
    }

    /**
     * Create a Vendor from a Callable
     */
    public static<T> Vendor<T> vendor(Callable<T> f) {
	return j.vendor(f);
    }

    /**
     * Create a Vendor from a value
     */
    public static<T> Vendor<T> vendor(T v) {
	return j.vendor(v);
    }
}