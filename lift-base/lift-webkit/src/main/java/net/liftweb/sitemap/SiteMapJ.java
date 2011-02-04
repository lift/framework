/*
 * Copyright 2011 WorldWide Conferencing, LLC
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

package net.liftweb.sitemap;

/**
 * The bridge from Java-land into SiteMap
 */
public final class SiteMapJ {
    private final static SiteMapSingleton j = 
	(new SiteMapJBridge()).siteMap();

    /**
     * Get the SiteMap singleton
     */
    public static SiteMapSingleton j() {
	return j;
    }

    /**
     * Given a bunch of Menu items, create a
     * SiteMap
     */
    public static SiteMap build(ConvertableToMenu... m) {
	return j().build(m);
    }
				
}