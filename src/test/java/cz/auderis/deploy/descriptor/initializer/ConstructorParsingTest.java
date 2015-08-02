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

import cz.auderis.test.category.UnitTest;
import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.categories.Category;

import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class ConstructorParsingTest {

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseEmptyConstructor() throws Exception {
		// Given
		final Source xml = xml("<constructor></constructor>");

		// When
		final Object parsedObject = xmlParser.unmarshal(xml);

		// Then
		assertThat(parsedObject, instanceOf(ConstructorElement.class));
		final ConstructorElement constr = (ConstructorElement) parsedObject;
		assertThat(constr.getArguments(), hasSize(0));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseNormalConstructor() throws Exception {
		// Given
		final Source xml = xml("<constructor>"
				+ "<argument property=\"x\" />"
				+ "<argument property=\"y123\" />"
				+ "</constructor>");

		// When
		final Object parsedObject = xmlParser.unmarshal(xml);

		// Then
		assertThat(parsedObject, instanceOf(ConstructorElement.class));
		final ConstructorElement constr = (ConstructorElement) parsedObject;
		assertThat(constr.getArguments(), hasSize(2));
		assertThat(constr.getArguments().get(0), hasProperty("propertyName", is("x")));
		assertThat(constr.getArguments().get(1), hasProperty("propertyName", is("y123")));
	}

}
