/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.jena.rdfxml.xmlinput0.libtest;

import java.io.*;

/**
 *In test cases we cannot open all the input files
 * while creating the test suite, but must defer the
 * opening until the test is actually run.
 */
abstract class LazyInputStream extends InputStream {

    private InputStream underlying;
    abstract InputStream open() throws IOException;

    boolean connect() throws IOException {
    	if ( underlying != null )
    	  return true;
    	else {
            underlying = open();
    	}
    	return underlying != null;

    }


    @Override
    public int read() throws IOException {
        if (underlying == null)
            underlying = open();
        return underlying.read();
    }

    @Override
    public void close() throws IOException {
        if (underlying != null) {
            underlying.close();
            underlying = null;
        }
    }



}
