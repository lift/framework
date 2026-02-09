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

package net.liftweb.http;

/**
 * The Java interface to SessionVar, RequestVar, ContainerVar,
 * and TransientRequestVar
 */

import java.util.concurrent.Callable;

/**
 * Vend SessionVar, RequestVar, etc. to Java callers
 */
public class VarsJ {
    /**
     * Vend a SessionVar with the default value
     */
    public static<T> SessionVar<T> vendSessionVar(T defValue) {
	return (new VarsJBridge()).vendSessionVar(defValue, new Exception());
    }

    /**
     * Vend a SessionVar with the function to create the default value
     */
    public static<T>  SessionVar<T> vendSessionVar(Callable<T> defFunc) {
	return (new VarsJBridge()).vendSessionVar(defFunc, new Exception());
    }
}