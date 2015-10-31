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

import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.dependency.PropertyInjectionElement;
import cz.auderis.test.category.SanityTest;
import cz.auderis.test.category.UnitTest;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.ExpectedException;
import org.xml.sax.SAXException;

import javax.xml.bind.UnmarshalException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import java.util.List;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SetBeanParsingTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private static final String BEAN_START = "<set name=\"x\">";
	private static final String BEAN_END = "</set>";

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldRequireNameAttribute() throws Exception {
		// Given
		final Source xml = xml("<set></set>");
		expectedException.expect(UnmarshalException.class);
		expectedException.reportMissingExceptionWithMessage("Set bean without name attribute was parsed");
		// When / Then
		xmlParser.unmarshal(xml);
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanName() throws Exception {
		// Given
		final Source xml = xml("<set name=\"xyz\" />");
		// When
		final StandaloneSetBean bean = (StandaloneSetBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getName(), is("xyz"));
		assertThat(bean.getBeanType(), is(BeanType.SET));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseItemClass() throws Exception {
		// Given
		final Source xml = xml("<set name=\"xyz\" itemClass=\"com.test.Type\"></set>");
		// When
		final StandaloneSetBean bean = (StandaloneSetBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getItemClassName(), is("com.test.Type"));
	}


	@Test
	@Category(UnitTest.class)
	public void shouldParseTextInjection() throws Exception {
		// Given
		final Source xml = setWithItems("ABCD9876x");
		// When
		final StandaloneSetBean bean = (StandaloneSetBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getItems(), hasSize(1));
		final List<Object> contents = bean.getItems().get(0).getContents();
		assertThat(contents, hasSize(1));
		assertThat(contents.get(0), is((Object) "ABCD9876x"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanPropertyInjection() throws Exception {
		// Given
		final Source xml = setWithItems("<inject bean=\"klm\" />");
		// When
		final StandaloneSetBean bean = (StandaloneSetBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getItems(), hasSize(1));
		final List<Object> contents = bean.getItems().get(0).getContents();
		assertThat(contents, hasSize(1));
		assertThat(contents.get(0), instanceOf(BeanInjectionElement.class));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseMixedInjections() throws Exception {
		// Given
		final Source xml = setWithItems(
				"<inject bean=\"klm\" />",
				"12345",
				"<injectProperty bean=\"pqr\" property=\"prop3\" />"
		);
		// When
		final StandaloneSetBean bean = (StandaloneSetBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getItems(), hasSize(3));
		assertThat(bean.getItems().get(0).getContents(), hasSize(1));
		assertThat(bean.getItems().get(1).getContents(), hasSize(1));
		assertThat(bean.getItems().get(2).getContents(), hasSize(1));
		assertThat(bean.getItems().get(0).getContents().get(0), instanceOf(BeanInjectionElement.class));
		assertThat(bean.getItems().get(1).getContents().get(0), is((Object) "12345"));
		assertThat(bean.getItems().get(2).getContents().get(0), instanceOf(PropertyInjectionElement.class));
	}

	private static Source setWithItems(String... items) throws SAXException {
		final StringBuilder str = new StringBuilder(2048);
		str.append(BEAN_START);
		for (final String part : items) {
			str.append("<item>").append(part).append("</item>");
		}
		str.append(BEAN_END);
		final Source result = xml(str.toString());
		return result;
	}

}
