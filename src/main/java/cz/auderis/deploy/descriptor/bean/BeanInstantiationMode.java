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

package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.DescriptorParserSupport;

import java.util.Set;

public enum BeanInstantiationMode implements DescriptorParserSupport.NamedEnum {

	ON_DEMAND("OnDemand", "Auto", "Optional", "On Demand", "on-demand", "Lazy"),

	ALWAYS("Always", "Required", "Mandatory", "Eager"),

	DISABLED("Disabled", "Never", "Forbidden")

	;

	private static BeanInstantiationMode DEFAULT_MODE = ON_DEMAND;
	private final String canonicalName;
	private final Set<String> recognizedNames;

	BeanInstantiationMode(String canonicalName, String... otherNames) {
		this.canonicalName = canonicalName;
		this.recognizedNames = DescriptorParserSupport.recognizedNameSet(canonicalName, otherNames);
	}

	public static BeanInstantiationMode getDefaultMode() {
		return DEFAULT_MODE;
	}

	public static void setDefaultMode(BeanInstantiationMode newDefaultMode) {
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
