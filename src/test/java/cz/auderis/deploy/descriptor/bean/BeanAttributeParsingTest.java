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

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import static cz.auderis.deploy.XmlSupport.createLenientParser;
import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.namedEnumAliases;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

@RunWith(JUnitParamsRunner.class)
public class BeanAttributeParsingTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(source = BeanInstantiationMode.class)
	public void shouldParseBeanInstantiationModeInStrictMode(BeanInstantiationMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		final Source xml = xml("<bean name=\"x\" class=\"y\" mode=\"" + mode.getCanonicalName() + "\" />");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(NormalBean.class));
		final NormalBean bean = (NormalBean) parsedObj;
		final BeanInstantiationMode parsedMode = bean.getInstantiationMode();
		assertThat("Parsing instantiation mode " + mode, parsedMode, is(mode));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(method = "nonCanonicalInstantiationModeAliases")
	public void shouldNotParseBeanInstantiationModeAliasesInStrictMode(String alias, BeanInstantiationMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		final Source xml = xml("<bean name=\"x\" class=\"y\" mode=\"" + alias + "\" />");
		expectedException.expect(JAXBException.class);
		expectedException.reportMissingExceptionWithMessage("Parsed despite invalid name " + alias + " for mode " + mode);
		// When / Then
		xmlParser.unmarshal(xml);
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(method = "instantiationModeAliases")
	public void shouldParseBeanInstantiationModeAliasesInLenientMode(String name, BeanInstantiationMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		xmlParser = createLenientParser();
		final Source xml = xml("<bean name=\"x\" class=\"y\" mode=\" " + name.toUpperCase() + " \" />");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(NormalBean.class));
		final NormalBean bean = (NormalBean) parsedObj;
		final BeanInstantiationMode parsedMode = bean.getInstantiationMode();
		assertThat("Parsing instantiation mode " + name, parsedMode, is(mode));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(source = BeanConflictMode.class)
	public void shouldParseBeanConflictModeInStrictMode(BeanConflictMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		final Source xml = xml("<bean name=\"x\" class=\"y\" conflict=\"" + mode.getCanonicalName() + "\" />");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(NormalBean.class));
		final NormalBean bean = (NormalBean) parsedObj;
		final BeanConflictMode parsedMode = bean.getConflictResolutionMode();
		assertThat("Parsing conflict mode " + mode, parsedMode, is(mode));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(method = "nonCanonicalConflictModeAliases")
	public void shouldNotParseBeanConflictModeAliasesInStrictMode(String alias, BeanConflictMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		final Source xml = xml("<bean name=\"x\" class=\"y\" conflict=\"" + alias + "\" />");
		expectedException.expect(JAXBException.class);
		expectedException.reportMissingExceptionWithMessage("Parsed despite invalid name " + alias + " for mode " + mode);
		// When / Then
		xmlParser.unmarshal(xml);
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(method = "conflictModeAliases")
	public void shouldParseBeanConflictModeAliasesInLenientMode(String name, BeanConflictMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		xmlParser = createLenientParser();
		final Source xml = xml("<bean name=\"x\" class=\"y\" conflict=\" " + name.toUpperCase() + " \" />");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(NormalBean.class));
		final NormalBean bean = (NormalBean) parsedObj;
		final BeanConflictMode parsedMode = bean.getConflictResolutionMode();
		assertThat("Parsing conflict mode " + name, parsedMode, is(mode));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(source = ExternalBundleSourceMode.class)
	public void shouldParseBundleSourceModeInStrictMode(ExternalBundleSourceMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		final Source xml = xml("<propertyBundle name=\"x\" useResource=\"" + mode.getCanonicalName() + "\" />");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(ExternalBundleBean.class));
		final ExternalBundleBean bean = (ExternalBundleBean) parsedObj;
		final ExternalBundleSourceMode parsedMode = bean.getSourceMode();
		assertThat("Parsing bundle source mode " + mode, parsedMode, is(mode));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(method = "nonCanonicalExternalBundleSourceModeAliases")
	public void shouldNotParseBundleSourceModeAliasesInStrictMode(String alias, ExternalBundleSourceMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		final Source xml = xml("<propertyBundle name=\"x\" useResource=\"" + alias + "\" />");
		expectedException.expect(JAXBException.class);
		expectedException.reportMissingExceptionWithMessage("Parsed despite invalid name " + alias + " for mode " + mode);
		// When / Then
		xmlParser.unmarshal(xml);
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters(method = "externalBundleSourceModeAliases")
	public void shouldParseBundleSourceModeAliasesInLenientMode(String name, ExternalBundleSourceMode mode) throws Exception {
		// Given
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		xmlParser = createLenientParser();
		final Source xml = xml("<propertyBundle name=\"x\" useResource=\" " + name.toUpperCase() + " \" />");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(ExternalBundleBean.class));
		final ExternalBundleBean bean = (ExternalBundleBean) parsedObj;
		final ExternalBundleSourceMode parsedMode = bean.getSourceMode();
		assertThat("Parsing bundle source mode " + name, parsedMode, is(mode));
	}

	Object[] instantiationModeAliases() {
		return namedEnumAliases(BeanInstantiationMode.class, true);
	}

	Object[] nonCanonicalInstantiationModeAliases() {
		return namedEnumAliases(BeanInstantiationMode.class, false);
	}

	Object[] conflictModeAliases() {
		return namedEnumAliases(BeanConflictMode.class, true);
	}

	Object[] nonCanonicalConflictModeAliases() {
		return namedEnumAliases(BeanConflictMode.class, false);
	}

	Object[] externalBundleSourceModeAliases() {
		return namedEnumAliases(ExternalBundleSourceMode.class, true);
	}

	Object[] nonCanonicalExternalBundleSourceModeAliases() {
		return namedEnumAliases(ExternalBundleSourceMode.class, false);
	}

}
