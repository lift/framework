/*
 * Copyright 2011-2026 Lift Committers and Contributors
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

import scala.xml.NodeSeq;
import scala.Function1;

/**
 * The bridge from Java to Lift's CSS Selector Transforms
 */
public final class Css {
    private static CssJBridge j = new CssJBridge();

    /**
     * Create a Css Selector Transform 
     */
    public static CssSel sel(String selector, String value) {
	return j.sel(selector, value);
    }
    
    /**
     * Create a Css Selector Transform 
     */
    public static CssSel sel(String selector, NodeSeq value) {
	return j.sel(selector, value);
    }
    
    /**
     * Create a Css Selector Transform 
     */
    public static CssSel sel(String selector, Function1<NodeSeq, NodeSeq> value) {
	return j.sel(selector, value);
    }
    
    /**
     * Create a Css Selector Transform 
     */
    public static CssSel sel(String selector, Bindable value) {
	return j.sel(selector, value);
    }
}