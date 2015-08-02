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
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

public class ListBeanParsingTest {

	private static final String BEAN_START = "<list name=\"x\">";
	private static final String BEAN_END = "</list>";

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category(UnitTest.class)
	public void shouldRequireNameAttribute() throws Exception {
		try {
			final Source xml = xml("<list></list>");
			xmlParser.unmarshal(xml);
			fail("List bean without name attribute was parsed");
		} catch (UnmarshalException e) {
			// OK, expected
		}
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanName() throws Exception {
		// Given
		final Source xml = xml("<list name=\"xyz\" />");
		// When
		final StandaloneListBean bean = (StandaloneListBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getName(), is("xyz"));
		assertThat(bean.getBeanType(), is(BeanType.LIST));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseItemClass() throws Exception {
		// Given
		final Source xml = xml("<list name=\"xyz\" itemClass=\"com.test.Type\"></list>");
		// When
		final StandaloneListBean bean = (StandaloneListBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getItemClassName(), is("com.test.Type"));
	}


	@Test
	@Category(UnitTest.class)
	public void shouldParseTextInjection() throws Exception {
		// Given
		final Source xml = listWithItems("ABCD9876x");
		// When
		final StandaloneListBean bean = (StandaloneListBean) xmlParser.unmarshal(xml);
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
		final Source xml = listWithItems("<inject bean=\"klm\" />");
		// When
		final StandaloneListBean bean = (StandaloneListBean) xmlParser.unmarshal(xml);
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
		final Source xml = listWithItems(
				"<inject bean=\"klm\" />",
				"12345",
				"<inject-property bean=\"pqr\" property=\"prop3\" />"
		);
		// When
		final StandaloneListBean bean = (StandaloneListBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getItems(), hasSize(3));
		assertThat(bean.getItems().get(0).getContents(), hasSize(1));
		assertThat(bean.getItems().get(1).getContents(), hasSize(1));
		assertThat(bean.getItems().get(2).getContents(), hasSize(1));
		assertThat(bean.getItems().get(0).getContents().get(0), instanceOf(BeanInjectionElement.class));
		assertThat(bean.getItems().get(1).getContents().get(0), is((Object) "12345"));
		assertThat(bean.getItems().get(2).getContents().get(0), instanceOf(PropertyInjectionElement.class));
	}

	private static Source listWithItems(String... items) throws SAXException {
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
