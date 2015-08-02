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

package cz.auderis.deploy.descriptor.dependency;

import cz.auderis.deploy.descriptor.DescriptorParserSupport;

import java.util.Set;

public enum MaterializationMode implements DescriptorParserSupport.NamedEnum {

	CONSTRUCTED_BEAN("Early", "AfterConstruction", "After Construction", "after-construction", "Incomplete"),
	FINAL_BEAN("Normal", "Final", "Initialized", "Complete"),

	;

	private static MaterializationMode DEFAULT_MODE = FINAL_BEAN;
	private final String canonicalName;
	private final Set<String> recognizedNames;

	MaterializationMode(String canonicalName, String... otherNames) {
		this.canonicalName = canonicalName;
		this.recognizedNames = DescriptorParserSupport.recognizedNameSet(canonicalName, otherNames);
	}

	public static MaterializationMode getDefaultMode() {
		return DEFAULT_MODE;
	}

	public static void setDefaultMode(MaterializationMode newDefaultMode) {
		if (null == newDefaultMode) {
			throw new NullPointerException();
		}
		DEFAULT_MODE = newDefaultMode;
	}

	@Override
	public String getCanonicalName() {
		return canonicalName;
	}

	@Override
	public Set<String> getRecognizedNames() {
		return recognizedNames;
	}

	@Override
	public String toString() {
		return canonicalName;
	}

}
