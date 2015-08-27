/*
 * Copyright 2015 Boleslav Bobcik - Auderis
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package cz.auderis.deploy.processor.validator;

import java.util.regex.Pattern;

public interface ValidationPattern {

	Pattern BEAN_NAME = Pattern.compile("[\\p{L}\\p{N}._:-]+");

	Pattern PROPERTY_NAME = Pattern.compile("[\\p{L}_$][\\p{L}\\p{N}_$]*");

	Pattern JAVA_CLASS = Pattern.compile("([\\p{L}_$][\\p{L}\\p{N}_$]*\\.)*[\\p{L}_$][\\p{L}\\p{N}_$]*");


}
