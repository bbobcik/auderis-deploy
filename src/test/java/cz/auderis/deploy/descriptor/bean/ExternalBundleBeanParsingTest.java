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

import cz.auderis.deploy.descriptor.initializer.ExternalResourceElement;
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
import java.util.List;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

@RunWith(JUnitParamsRunner.class)
public class ExternalBundleBeanParsingTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private static final String BEAN_START = "<propertyBundle name=\"xyz\">";
	private static final String BEAN_END = "</propertyBundle>";

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldRequireNameAttribute() throws Exception {
		// Given
		final Source xml = xml("<propertyBundle></propertyBundle>");
		expectedException.expect(UnmarshalException.class);
		expectedException.reportMissingExceptionWithMessage("Property bundle bean without name attribute was parsed");
		// When / Then
		xmlParser.unmarshal(xml);
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanName() throws Exception {
		// Given
		final Source xml = xml("<propertyBundle name=\"xyz\"></propertyBundle>");
		// When
		final ExternalBundleBean bean = (ExternalBundleBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getName(), is("xyz"));
		assertThat(bean.getBeanType(), is(BeanType.EXTERNAL_BUNDLE));
	}

	@Test
	@Category(UnitTest.class)
	@Parameters(source = ExternalBundleSourceMode.class)
	public void shouldParseBundleMode(ExternalBundleSourceMode mode) throws Exception {
		// Given
		final Source xml = xml("<propertyBundle name=\"xyz\" useResource=\"" + mode.getCanonicalName() + "\" />");
		// When
		final ExternalBundleBean bean = (ExternalBundleBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getSourceMode(), is(mode));
	}


	@Test
	@Category(UnitTest.class)
	public void shouldParseSingleResourceSpecification() throws Exception {
		// Given
		final Source xml = bundle("spec1");
		// When
		final ExternalBundleBean bean = (ExternalBundleBean) xmlParser.unmarshal(xml);
		// Then
		final List<ExternalResourceElement> resources = bean.getResources();
		assertThat(resources, hasSize(1));
		final ExternalResourceElement firstResource = resources.get(0);
		assertThat(firstResource.getResourceSpecification(), is("spec1"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseMultipleResourceSpecifications() throws Exception {
		// Given
		final Source xml = bundle("spec1", "spec2", "specX");
		// When
		final ExternalBundleBean bean = (ExternalBundleBean) xmlParser.unmarshal(xml);
		// Then
		final List<ExternalResourceElement> resources = bean.getResources();
		assertThat(resources, hasSize(3));
		assertThat(resources.get(0).getResourceSpecification(), is("spec1"));
		assertThat(resources.get(1).getResourceSpecification(), is("spec2"));
		assertThat(resources.get(2).getResourceSpecification(), is("specX"));
	}

	private static Source bundle(String... contents) throws SAXException {
		final StringBuilder str = new StringBuilder(2048);
		str.append(BEAN_START);
		for (final String part : contents) {
			str.append("<resource>").append(part).append("</resource>");
		}
		str.append(BEAN_END);
		final Source result = xml(str.toString());
		return result;
	}

}
