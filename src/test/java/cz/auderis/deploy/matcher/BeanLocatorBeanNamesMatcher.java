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

package cz.auderis.deploy.matcher;

import cz.auderis.deploy.parser.BeanLocatorVisitor;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class BeanLocatorBeanNamesMatcher<T extends BeanLocatorVisitor> extends TypeSafeMatcher<T> {

	private final Set<String> expectedNames;
	private final boolean allowOthers;

	public BeanLocatorBeanNamesMatcher(String... names) {
		this(true, names);
	}

	public BeanLocatorBeanNamesMatcher(boolean allowOthers, String... names) {
		super(BeanLocatorVisitor.class);
		this.allowOthers = allowOthers;
		if (0 == names.length) {
			expectedNames = Collections.emptySet();
		} else {
			this.expectedNames = new HashSet<String>(names.length);
			Collections.addAll(expectedNames, names);
		}
	}

	@Override
	protected boolean matchesSafely(BeanLocatorVisitor locator) {
		final Set<String> locatedNames = locator.getLocatedBeanNames();
		if (locatedNames.equals(expectedNames)) {
			return true;
		} else if (allowOthers && locatedNames.containsAll(expectedNames)) {
			return true;
		}
		return false;
	}

	@Override
	public void describeTo(Description description) {
		if (allowOthers) {
			description.appendText("a locator");
			if (!expectedNames.isEmpty()) {
				description.appendText(" that located beans ");
				description.appendValueList("", ", ", "", expectedNames);
			}
		} else if (expectedNames.isEmpty()) {
			description.appendText("a locator that located no beans");
		} else {
			description.appendText("a locator that located only beans ");
			description.appendValueList("", ", ", "", expectedNames);
		}
	}

	@Override
	protected void describeMismatchSafely(BeanLocatorVisitor locator, Description description) {
		final Set<String> locatedNames = locator.getLocatedBeanNames();
		final Set<String> missingNames = new HashSet<String>(expectedNames);
		missingNames.removeAll(locatedNames);
		final boolean hasMissing = !missingNames.isEmpty();
		if (hasMissing) {
			description.appendText("did not locate beans ");
			description.appendValueList("", ", ", "", missingNames);
		}
		if (!allowOthers && !expectedNames.equals(locatedNames)) {
			final Set<String> unexpectedNames = new HashSet<String>(locatedNames);
			unexpectedNames.removeAll(expectedNames);
			if (hasMissing) {
				description.appendText(" and ");
			}
			description.appendText("found unexpected beans ");
			description.appendValueList("", ", ", "", unexpectedNames);
		}
	}

}
