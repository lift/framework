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

package net.liftweb.actor;


public class MyJavaActor extends JavaActor {
    private int myValue = 0;

    @Receive protected void set(Set what) {
	myValue = what.num();
    }

    @Receive public void get(Get get) {
	reply(new Answer(myValue));
    }

    @Receive protected Answer add(Add toAdd) {
	myValue += toAdd.num();
	return new Answer(myValue);
    }

    @Receive public Answer sub(Sub toSub) {
	myValue -= toSub.num();
	return new Answer(myValue);
    }
}
