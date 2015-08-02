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
import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.categories.Category;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import static cz.auderis.deploy.XmlSupport.createLenientParser;
import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

public class BeanAttributeParsingTest {

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBeanInstantiationModeInStrictMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		for (BeanInstantiationMode mode : BeanInstantiationMode.values()) {
			// Given
			final Source xml = xml("<bean name=\"x\" class=\"y\" mode=\"" + mode.getCanonicalName() + "\" />");
			// When
			final Object parsedObj = xmlParser.unmarshal(xml);
			// Then
			assertThat(parsedObj, instanceOf(NormalBean.class));
			final NormalBean bean = (NormalBean) parsedObj;
			final BeanInstantiationMode parsedMode = bean.getInstantiationMode();
			assertThat("Parsing instantiation mode " + mode, parsedMode, is(mode));
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldNotParseBeanInstantiationModeAliasesInStrictMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		for (BeanInstantiationMode mode : BeanInstantiationMode.values()) {
			for (final String name : mode.getRecognizedNames()) {
				// Given
				if (name.equals(mode.getCanonicalName())) {
					continue;
				}
				final Source xml = xml("<bean name=\"x\" class=\"y\" mode=\"" + name + "\" />");
				// When
				try {
					xmlParser.unmarshal(xml);
					fail("Parsed despite invalid name " + name + " for mode " + mode);
				} catch (JAXBException e) {
					// ok
				}
			}
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBeanInstantiationModeAliasesInLenientMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		xmlParser = createLenientParser();
		for (BeanInstantiationMode mode : BeanInstantiationMode.values()) {
			for (final String name : mode.getRecognizedNames()) {
				// Given
				// Add spaces on both sides and convert to uppercase
				final Source xml = xml("<bean name=\"x\" class=\"y\" mode=\" " + name.toUpperCase() + " \" />");
				// When
				final Object parsedObj = xmlParser.unmarshal(xml);
				assertThat(parsedObj, instanceOf(NormalBean.class));
				final NormalBean bean = (NormalBean) parsedObj;
				final BeanInstantiationMode parsedMode = bean.getInstantiationMode();
				assertThat("Parsing instantiation mode " + name, parsedMode, is(mode));
			}
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBeanConflictModeInStrictMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		for (BeanConflictMode mode : BeanConflictMode.values()) {
			// Given
			final Source xml = xml("<bean name=\"x\" class=\"y\" conflict=\"" + mode.getCanonicalName() + "\" />");
			// When
			final Object parsedObj = xmlParser.unmarshal(xml);
			// Then
			assertThat(parsedObj, instanceOf(NormalBean.class));
			final NormalBean bean = (NormalBean) parsedObj;
			final BeanConflictMode parsedMode = bean.getConflictResolutionMode();
			assertThat("Parsing conflict mode " + mode, parsedMode, is(mode));
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldNotParseBeanConflictModeAliasesInStrictMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		for (BeanConflictMode mode : BeanConflictMode.values()) {
			for (final String name : mode.getRecognizedNames()) {
				// Given
				if (name.equals(mode.getCanonicalName())) {
					continue;
				}
				final Source xml = xml("<bean name=\"x\" class=\"y\" conflict=\"" + name + "\" />");
				// When
				try {
					xmlParser.unmarshal(xml);
					fail("Parsed despite invalid name " + name + " for mode " + mode);
				} catch (JAXBException e) {
					// ok
				}
			}
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBeanConflictModeAliasesInLenientMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		xmlParser = createLenientParser();
		for (BeanConflictMode mode : BeanConflictMode.values()) {
			for (final String name : mode.getRecognizedNames()) {
				// Given
				// Add spaces on both sides and convert to uppercase
				final Source xml = xml("<bean name=\"x\" class=\"y\" conflict=\" " + name.toUpperCase() + " \" />");
				// When
				final Object parsedObj = xmlParser.unmarshal(xml);
				assertThat(parsedObj, instanceOf(NormalBean.class));
				final NormalBean bean = (NormalBean) parsedObj;
				final BeanConflictMode parsedMode = bean.getConflictResolutionMode();
				assertThat("Parsing conflict mode " + name, parsedMode, is(mode));
			}
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBundleSourceModeInStrictMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		for (ExternalBundleSourceMode mode : ExternalBundleSourceMode.values()) {
			// Given
			final Source xml = xml("<propertyBundle name=\"x\" useResource=\"" + mode.getCanonicalName() + "\" />");
			// When
			final Object parsedObj = xmlParser.unmarshal(xml);
			// Then
			assertThat(parsedObj, instanceOf(ExternalBundleBean.class));
			final ExternalBundleBean bean = (ExternalBundleBean) parsedObj;
			final ExternalBundleSourceMode parsedMode = bean.getSourceMode();
			assertThat("Parsing bundle source mode " + mode, parsedMode, is(mode));
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldNotParseBundleSourceModeAliasesInStrictMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		for (ExternalBundleSourceMode mode : ExternalBundleSourceMode.values()) {
			for (final String name : mode.getRecognizedNames()) {
				// Given
				if (name.equals(mode.getCanonicalName())) {
					continue;
				}
				final Source xml = xml("<propertyBundle name=\"x\" useResource=\"" + name + "\" />");
				// When
				try {
					xmlParser.unmarshal(xml);
					fail("Parsed despite invalid name " + name + " for mode " + mode);
				} catch (JAXBException e) {
					// ok
				}
			}
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBundleSourceModeAliasesInLenientMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		xmlParser = createLenientParser();
		//
		for (ExternalBundleSourceMode mode : ExternalBundleSourceMode.values()) {
			for (final String name : mode.getRecognizedNames()) {
				// Given
				// Add spaces on both sides and convert to uppercase
				final Source xml = xml("<propertyBundle name=\"x\" useResource=\" " + name.toUpperCase() + " \" />");
				// When
				final Object parsedObj = xmlParser.unmarshal(xml);
				assertThat(parsedObj, instanceOf(ExternalBundleBean.class));
				final ExternalBundleBean bean = (ExternalBundleBean) parsedObj;
				final ExternalBundleSourceMode parsedMode = bean.getSourceMode();
				assertThat("Parsing bundle source mode " + name, parsedMode, is(mode));
			}
		}
	}

}
