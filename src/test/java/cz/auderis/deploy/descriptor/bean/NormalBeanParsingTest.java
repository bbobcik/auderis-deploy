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

import cz.auderis.deploy.descriptor.initializer.ConstructorElement;
import cz.auderis.deploy.descriptor.initializer.PropertyElement;
import cz.auderis.test.category.UnitTest;
import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.xml.sax.SAXException;

import javax.xml.bind.UnmarshalException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import java.util.List;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

public class NormalBeanParsingTest {

	private static final String BEAN_START = "<bean name=\"xyz\" class=\"java.lang.Object\">";
	private static final String BEAN_END = "</bean>";

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category(UnitTest.class)
	public void shouldRequireNameAttribute() throws Exception {
		try {
			final Source xml = xml("<bean class=\"java.lang.Object\" ></bean>");
			xmlParser.unmarshal(xml);
			fail("Normal bean without name attribute was parsed");
		} catch (UnmarshalException e) {
			// OK, expected
		}
	}

	@Test
	@Category(UnitTest.class)
	public void shouldRequireClassAttribute() throws Exception {
		try {
			final Source xml = xml("<bean name=\"x\"></bean>");
			xmlParser.unmarshal(xml);
			fail("Normal bean without class attribute was parsed");
		} catch (UnmarshalException e) {
			// OK, expected
		}
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanName() throws Exception {
		// Given
		final Source xml = xml("<bean name=\"xyz\" class=\"java.lang.Object\" ></bean>");
		// When
		final NormalBean bean = (NormalBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getName(), is("xyz"));
		assertThat(bean.getBeanType(), is(BeanType.NORMAL));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanClass() throws Exception {
		// Given
		final Source xml = xml("<bean name=\"xyz\" class=\"com.test.Type\"></bean>");
		// When
		final NormalBean bean = (NormalBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getBeanClassName(), is("com.test.Type"));
	}


	@Test
	@Category(UnitTest.class)
	public void shouldParseTextPropertyInjection() throws Exception {
		// Given
		final Source xml = bean("<property name=\"prop1\">ABCD9876x</property>");
		// When
		final NormalBean bean = (NormalBean) xmlParser.unmarshal(xml);
		// Then
		final List<PropertyElement> properties = bean.getProperties();
		assertThat(properties, hasSize(1));
		final PropertyElement property = properties.get(0);
		assertThat(property.getName(), is("prop1"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanPropertyInjection() throws Exception {
		// Given
		final Source xml = bean("<property name=\"prop2\"><inject bean=\"klm\" /></property>");
		// When
		final NormalBean bean = (NormalBean) xmlParser.unmarshal(xml);
		// Then
		final List<PropertyElement> properties = bean.getProperties();
		assertThat(properties, hasSize(1));
		final PropertyElement property = properties.get(0);
		assertThat(property.getName(), is("prop2"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseConstructorDefinition() throws Exception {
		// Given
		final Source xml = bean("<constructor><argument property=\"prop2\" /></constructor>");
		// When
		final NormalBean bean = (NormalBean) xmlParser.unmarshal(xml);
		// Then
		final ConstructorElement constructor = bean.getConstructor();
		assertThat(constructor, notNullValue());
		assertThat(constructor.getArguments(), hasSize(1));
		assertThat(constructor.getArguments().get(0), hasProperty("propertyName", is("prop2")));
	}


	private static Source bean(String... contents) throws SAXException {
		final StringBuilder str = new StringBuilder(2048);
		str.append(BEAN_START);
		for (final String part : contents) {
			str.append(part);
		}
		str.append(BEAN_END);
		final Source result = xml(str.toString());
		return result;
	}

}
