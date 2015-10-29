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

import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.dependency.PropertyInjectionElement;
import cz.auderis.test.category.SanityTest;
import cz.auderis.test.category.UnitTest;
import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.xml.sax.SAXException;

import javax.xml.bind.UnmarshalException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

@RunWith(JUnitParamsRunner.class)
public class PropertyElementParsingTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldRequireNameAttribute() throws Exception {
		// Given
		final Source xml = xml("<property />");
		expectedException.expect(UnmarshalException.class);
		expectedException.reportMissingExceptionWithMessage("Property without name attribute was parsed");
		// When / Then
		xmlParser.unmarshal(xml);
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParsePropertyName() throws Exception {
		// Given
		final Source xml = xml("<property name=\"x1y2\" />");
		// When
		final PropertyElement property = (PropertyElement) xmlParser.unmarshal(xml);
		// Then
		assertThat(property.name, is("x1y2"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseEmptyContents() throws Exception {
		// Given
		final Source xml = xml("<property name=\"x\"></property>");
		// When
		final PropertyElement property = (PropertyElement) xmlParser.unmarshal(xml);
		// Then
		assertThat(property.contents, hasSize(0));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseTextContents() throws Exception {
		// Given
		final Source xml = property("Simple text 123");
		// When
		final PropertyElement property = (PropertyElement) xmlParser.unmarshal(xml);
		// Then
		assertThat(property.contents, hasSize(1));
		final Object value = property.contents.get(0);
		assertThat(value, instanceOf(String.class));
		String txt = (String) value;
		assertThat(txt, is("Simple text 123"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanInjection() throws Exception {
		// Given
		final Source xml = property("<inject bean=\"pqr\" />");
		// When
		final PropertyElement property = (PropertyElement) xmlParser.unmarshal(xml);
		// Then
		assertThat(property.contents, hasSize(1));
		final Object value = property.contents.get(0);
		assertThat(value, instanceOf(BeanInjectionElement.class));
		final BeanInjectionElement injection = (BeanInjectionElement) value;
		assertThat(injection.getBeanName(), is("pqr"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseAutoWireBeanInjection() throws Exception {
		// Given
		final Source xml = property("<inject />");
		// When
		final PropertyElement property = (PropertyElement) xmlParser.unmarshal(xml);
		// Then
		assertThat(property.contents, hasSize(1));
		final Object value = property.contents.get(0);
		assertThat(value, instanceOf(BeanInjectionElement.class));
		final BeanInjectionElement injection = (BeanInjectionElement) value;
		assertThat(injection.getBeanName(), nullValue());
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParsePropertyInjection() throws Exception {
		// Given
		final Source xml = property("<injectProperty bean=\"pqr2\" property=\"prop2\" />");
		// When
		final PropertyElement property = (PropertyElement) xmlParser.unmarshal(xml);
		// Then
		assertThat(property.contents, hasSize(1));
		final Object value = property.contents.get(0);
		assertThat(value, instanceOf(PropertyInjectionElement.class));
		final PropertyInjectionElement injection = (PropertyInjectionElement) value;
		assertThat(injection.getBeanName(), is("pqr2"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseMixedContentsInjection() throws Exception {
		// Given
		final Source xml = property("Abc<inject bean=\"pqr\" />Def");
		// When
		final PropertyElement property = (PropertyElement) xmlParser.unmarshal(xml);
		// Then
		assertThat(property.contents, hasSize(3));
		assertThat(property.contents.get(0), is((Object) "Abc"));
		assertThat(property.contents.get(1), instanceOf(BeanInjectionElement.class));
		assertThat(property.contents.get(2), is((Object) "Def"));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters({
			"<inject bean=\"a\" /><inject bean=\"b\" />",
			"<inject bean=\"a\" /><inject-property bean=\"b\" property=\"prop\" />",
			"<inject-property bean=\"a\" property=\"x\" /><inject-property bean=\"b\" property=\"prop\" />"
	})
	public void shouldNotAllowMultipleInjections(String propertyContents) throws Exception {
		// Given
		expectedException.expect(UnmarshalException.class);
		expectedException.reportMissingExceptionWithMessage("Multiple injections parsed");
		final Source xml = property(propertyContents);
		// When / Then
		xmlParser.unmarshal(xml);
	}


	private static Source property(String contents) throws SAXException {
		return xml("<property name=\"x\">" + contents + "</property>");
	}

}
