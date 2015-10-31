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

package cz.auderis.deploy.processor.validator;

import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.deploy.descriptor.visitor.DefaultVisitorContext;
import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;

import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import java.util.List;
import java.util.Locale;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import static cz.auderis.deploy.XmlSupport.createLenientParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertThat;

@RunWith(JUnitParamsRunner.class)
public class SyntaxCheckVisitorTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private Unmarshaller xmlParser;
	private DefaultVisitorContext context;
	private SyntaxCheckVisitor visitor;

	@Before
	public void initializeSyntaxChecker() throws Exception {
		xmlParser = createLenientParser();
		context = new DefaultVisitorContext();
		//
		final ResourceBundle bundle = PropertyResourceBundle.getBundle(SyntaxCheckVisitor.DEFAULT_RESOURCE_BUNDLE, Locale.ENGLISH);
		visitor = new SyntaxCheckVisitor(context, bundle);
	}

	@Test
	@Parameters({ " ", "a/", "*b", "%d" })
	public void shouldDetectInvalidBeanName(String name) throws Exception {
		// Given
		final Source beanSource = xml("<bean name=\"" + name + "\" class=\"a.b.c\" />");
		final NormalBean bean = (NormalBean) xmlParser.unmarshal(beanSource);
		context.pushContextPart(bean);
		// When
		visitor.visitNormalBean(bean);
		// Then
		final List<SyntaxCheckMessage> messages = visitor.getMessages();
		assertThat(messages, hasSize(1));
		final SyntaxCheckMessage firstMessage = messages.get(0);
		assertThat(firstMessage.getMessage(), containsString("Invalid bean name '" + name + "' in its definition"));
	}


	@Test
	@Parameters({ " ", "a..b", "x.y.", ".klm" })
	public void shouldDetectInvalidBeanClass(String className) throws Exception {
		// Given
		final Source beanSource = xml("<bean name=\"x\" class=\"" + className + "\" />");
		final NormalBean bean = (NormalBean) xmlParser.unmarshal(beanSource);
		context.pushContextPart(bean);
		// When
		visitor.visitNormalBean(bean);
		// Then
		final List<SyntaxCheckMessage> messages = visitor.getMessages();
		assertThat(messages, hasSize(1));
		final SyntaxCheckMessage firstMessage = messages.get(0);
		assertThat(firstMessage.getMessage(), containsString("Invalid class name '" + className + "' in definition of bean 'x'"));
	}

}
