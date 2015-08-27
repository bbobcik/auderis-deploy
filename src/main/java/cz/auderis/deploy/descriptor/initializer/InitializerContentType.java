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

package cz.auderis.deploy.descriptor.initializer;

import cz.auderis.deploy.descriptor.DescriptorParsingException;
import cz.auderis.deploy.descriptor.dependency.AbstractInjectionElement;

import java.util.List;
import java.util.ListIterator;

import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isBlank;

public enum InitializerContentType {

	BEAN,
	BEAN_PROPERTY,
	TEXT,
	;


	public static InitializerContentType determineTypeFromMixedContents(List<Object> contents) throws DescriptorParsingException {
		final ListIterator<Object> partIterator = contents.listIterator();
		skipBlankItems(partIterator);
		if (!partIterator.hasNext()) {
			return InitializerContentType.TEXT;
		}
		final Object firstNonBlankItem = partIterator.next();
		if (firstNonBlankItem instanceof String) {
			// All remaining items must be strings as well
			while (partIterator.hasNext()) {
				final Object part = partIterator.next();
				if (!(part instanceof String)) {
					throw new DescriptorParsingException("Illegal mixed contents");
				}
			}
			return InitializerContentType.TEXT;
		}
		// Element-type contents found
		if (!(firstNonBlankItem instanceof AbstractInjectionElement)) {
			throw new DescriptorParsingException("Invalid contents: " + firstNonBlankItem);
		}
		final InitializerContentType determinedType;
		switch (((AbstractInjectionElement) firstNonBlankItem).getType()) {
			case BEAN:
				determinedType = BEAN;
				break;
			case PROPERTY:
				determinedType = BEAN_PROPERTY;
				break;
			default:
				throw new AssertionError();
		}
		// Only blanks may follow
		skipBlankItems(partIterator);
		if (partIterator.hasNext()) {
			final Object unexpectedItem = partIterator.next();
			if (unexpectedItem instanceof String) {
				throw new DescriptorParsingException("Illegal mixed contents");
			} else {
				throw new DescriptorParsingException("Unexpected contents after " + determinedType + ": " + unexpectedItem);
			}
		}
		return determinedType;

	}

	private static void skipBlankItems(ListIterator<Object> partIterator) {
		while (partIterator.hasNext()) {
			final Object nextPart = partIterator.next();
			final boolean isBlankString = ((nextPart instanceof String) && isBlank((String) nextPart));
			if (!isBlankString) {
				partIterator.previous();
				break;
			}
		}
	}

}
