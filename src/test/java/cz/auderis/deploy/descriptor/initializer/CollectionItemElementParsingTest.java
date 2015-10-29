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

import javax.xml.bind.JAXBException;
import javax.xml.bind.UnmarshalException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import static cz.auderis.deploy.XmlSupport.cdata;
import static cz.auderis.deploy.XmlSupport.createLenientParser;
import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.namedEnumAliases;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertThat;

@RunWith(JUnitParamsRunner.class)
public class CollectionItemElementParsingTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category(UnitTest.class)
	@Parameters(source = CollectionItemDependencyMode.class)
	public void shouldParseItemDependencyModeInStrictMode(CollectionItemDependencyMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		final Source xml = xml("<item dependencyMode=\"" + mode.getCanonicalName() + "\" />");

		// When
		final Object parsedObj = xmlParser.unmarshal(xml);

		// Then
		assertThat(parsedObj, instanceOf(CollectionItemElement.class));
		final CollectionItemElement item = (CollectionItemElement) parsedObj;
		final CollectionItemDependencyMode parsedMode = item.getDependencyMode();
		assertThat("Parsing dependency mode " + mode, parsedMode, is(mode));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(method = "nonCanonicalItemDependencyModeAliases")
	public void shouldNotParseItemDependencyModeAliasesInStrictMode(String alias, CollectionItemDependencyMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		final Source xml = xml("<item dependencyMode=\"" + alias + "\" />");
		expectedException.expect(JAXBException.class);
		expectedException.reportMissingExceptionWithMessage("Parsed despite invalid name " + alias + " for mode " + mode);

		// When / Then
		xmlParser.unmarshal(xml);
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(method = "itemDependencyModeAliases")
	public void shouldParseBeanInstantiationModeAliasesInLenientMode(String name, CollectionItemDependencyMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		xmlParser = createLenientParser();
		final Source xml = xml("<item dependencyMode=\" " + name.toUpperCase() + " \" />");

		// When
		final Object parsedObj = xmlParser.unmarshal(xml);

		// Then
		assertThat(parsedObj, instanceOf(CollectionItemElement.class));
		final CollectionItemElement item = (CollectionItemElement) parsedObj;
		final CollectionItemDependencyMode parsedMode = item.getDependencyMode();
		assertThat("Parsing dependency mode " + name, parsedMode, is(mode));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseEmptyItemAsText() throws Exception {
		// Given
		final Source xml = xml("<item />");

		// When
		final CollectionItemElement item = (CollectionItemElement) xmlParser.unmarshal(xml);

		// Then
		InitializerContentType contentType = InitializerContentType.determineTypeFromMixedContents(item.getContents());
		assertThat(contentType, is(InitializerContentType.TEXT));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseSimpleTextItem() throws Exception {
		// Given
		final Source xml = item("Abc 123");

		// When
		final CollectionItemElement item = (CollectionItemElement) xmlParser.unmarshal(xml);

		// Then
		InitializerContentType contentType = InitializerContentType.determineTypeFromMixedContents(item.getContents());
		assertThat(contentType, is(InitializerContentType.TEXT));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseCDataTextItem() throws Exception {
		// Given
		final Source xml = item(cdata("This is <CDATA> text"));

		// When
		final CollectionItemElement item = (CollectionItemElement) xmlParser.unmarshal(xml);

		// Then
		InitializerContentType contentType = InitializerContentType.determineTypeFromMixedContents(item.getContents());
		assertThat(contentType, is(InitializerContentType.TEXT));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseComplexTextItem() throws Exception {
		// Given
		final Source xml = item(cdata("This is <CDATA> text") + "\n xyz\r\n " + cdata(" another text ") + "\r and end ");

		// When
		final CollectionItemElement item = (CollectionItemElement) xmlParser.unmarshal(xml);

		// Then
		InitializerContentType contentType = InitializerContentType.determineTypeFromMixedContents(item.getContents());
		assertThat(contentType, is(InitializerContentType.TEXT));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanInjection() throws Exception {
		// Given
		final Source xml = item(" <inject /> ");

		// When
		final CollectionItemElement item = (CollectionItemElement) xmlParser.unmarshal(xml);

		// Then
		InitializerContentType contentType = InitializerContentType.determineTypeFromMixedContents(item.getContents());
		assertThat(contentType, is(InitializerContentType.BEAN));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParsePropertyInjection() throws Exception {
		// Given
		final Source xml = item(" <injectProperty bean=\"beanA\" property=\"num\" /> ");

		// When
		final CollectionItemElement item = (CollectionItemElement) xmlParser.unmarshal(xml);

		// Then
		InitializerContentType contentType = InitializerContentType.determineTypeFromMixedContents(item.getContents());
		assertThat(contentType, is(InitializerContentType.BEAN_PROPERTY));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldForbidTextAndElementContents() throws Exception {
		// Given
		final Source xml = item(" Abc <inject /> ");
		expectedException.expect(DescriptorParsingException.class);
		expectedException.expectMessage(startsWith("Illegal mixed contents"));

		// When / Then
		final CollectionItemElement item = (CollectionItemElement) xmlParser.unmarshal(xml);
		InitializerContentType contentType = InitializerContentType.determineTypeFromMixedContents(item.getContents());
	}

	@Test
	@Category(UnitTest.class)
	public void shouldForbidElementAndTextContents() throws Exception {
		// Given
		final Source xml = item(" <inject /> Def ");
		expectedException.expect(DescriptorParsingException.class);
		expectedException.expectMessage(startsWith("Illegal mixed contents"));

		// When / Then
		final CollectionItemElement item = (CollectionItemElement) xmlParser.unmarshal(xml);
		InitializerContentType contentType = InitializerContentType.determineTypeFromMixedContents(item.getContents());
	}

	@Test
	@Category(UnitTest.class)
	public void shouldForbidMultipleElements() throws Exception {
		// Given
		final Source xml = item(" <inject /> <inject /> ");
		expectedException.expect(DescriptorParsingException.class);
		expectedException.expectMessage(startsWith("Unexpected contents"));

		// When / Then
		final CollectionItemElement item = (CollectionItemElement) createLenientParser().unmarshal(xml);
		InitializerContentType contentType = InitializerContentType.determineTypeFromMixedContents(item.getContents());
	}

	@Test
	@Category(UnitTest.class)
	public void shouldFailOnUnknownElement() throws Exception {
		// Given
		final Source xml = item("<unknown />");
		expectedException.expect(UnmarshalException.class);

		// When / Then
		final CollectionItemElement item = (CollectionItemElement) xmlParser.unmarshal(xml);
	}

	Object[] itemDependencyModeAliases() {
		return namedEnumAliases(CollectionItemDependencyMode.class, true);
	}

	Object[] nonCanonicalItemDependencyModeAliases() {
		return namedEnumAliases(CollectionItemDependencyMode.class, false);
	}

	private static Source item(String itemContents) throws SAXException {
		return xml("<item>" + itemContents + "</item>");
	}

}
