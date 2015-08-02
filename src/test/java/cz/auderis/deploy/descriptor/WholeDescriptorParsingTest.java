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

package cz.auderis.deploy.descriptor;

import cz.auderis.test.category.RegressionTest;
import cz.auderis.test.category.SlowTest;
import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.xml.sax.InputSource;

import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import java.io.InputStream;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.filterSourceNamespace;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assume.assumeThat;

public class WholeDescriptorParsingTest {

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category({ RegressionTest.class, SlowTest.class })
	public void shouldParseSingleDeploymentDescriptor() throws Exception {
		// Given (perform test only if the resource exists)
		final InputStream deploymentStream = getClass().getResourceAsStream("deployment1.xml");
		assumeThat(deploymentStream, notNullValue());
		final Source deploymentSource = filterSourceNamespace(new InputSource(deploymentStream));

		// When
		final Object parsedObject = xmlParser.unmarshal(deploymentSource);

		// Then
		assertThat(parsedObject, instanceOf(Deployment.class));
	}

	@Test
	@Category({ RegressionTest.class, SlowTest.class })
	public void shouldParseDescriptorWithoutExplicitNamespace() throws Exception {
		// Given (perform test only if the resource exists)
		final InputStream deploymentStream = getClass().getResourceAsStream("deployment-without-ns.xml");
		assumeThat(deploymentStream, notNullValue());
		final Source deploymentSource = filterSourceNamespace(new InputSource(deploymentStream));

		// When
		final Object parsedObject = xmlParser.unmarshal(deploymentSource);

		// Then
		assertThat(parsedObject, instanceOf(Deployment.class));
	}


}
