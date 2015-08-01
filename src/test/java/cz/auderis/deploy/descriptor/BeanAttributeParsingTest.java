package cz.auderis.deploy.descriptor;

import cz.auderis.deploy.descriptor.bean.BeanConflictMode;
import cz.auderis.deploy.descriptor.bean.BeanInstantiationMode;
import cz.auderis.deploy.descriptor.bean.ExternalBundleBean;
import cz.auderis.deploy.descriptor.bean.ExternalBundleSourceMode;
import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.test.category.SanityTest;
import cz.auderis.test.category.UnitTest;
import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.categories.Category;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import java.io.Reader;

import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

public class BeanAttributeParsingTest {

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		final String packages = DescriptorParserSupport.getJaxbContextPackages();
		final JAXBContext jaxbCtx = JAXBContext.newInstance(packages);
		xmlParser = jaxbCtx.createUnmarshaller();
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBeanInstantiationModeInStrictMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.FALSE.toString());
		for (BeanInstantiationMode mode : BeanInstantiationMode.values()) {
			// Given
			final Reader xml = xml("<bean name=\"x\" mode=\"" + mode.getCanonicalName() + "\" />");
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
				final Reader xml = xml("<bean name=\"x\" mode=\"" + name + "\" />");
				// When
				final Object parsedObj;
				try {
					parsedObj = xmlParser.unmarshal(xml);
					fail("Parsed despite invalid name " + name + " for mode " + mode);
				} catch (DescriptorParsingException e) {
					// ok
				}
			}
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBeanInstantiationModeAliasesInLenientMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		for (BeanInstantiationMode mode : BeanInstantiationMode.values()) {
			for (final String name : mode.getRecognizedNames()) {
				// Given
				// Add spaces on both sides and convert to uppercase
				final Reader xml = xml("<bean name=\"x\" mode=\" " + name.toUpperCase() + " \" />");
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
			final Reader xml = xml("<bean name=\"x\" conflict=\"" + mode.getCanonicalName() + "\" />");
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
				final Reader xml = xml("<bean name=\"x\" conflict=\"" + name + "\" />");
				// When
				final Object parsedObj;
				try {
					parsedObj = xmlParser.unmarshal(xml);
					fail("Parsed despite invalid name " + name + " for mode " + mode);
				} catch (DescriptorParsingException e) {
					// ok
				}
			}
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBeanConflictModeAliasesInLenientMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		for (BeanConflictMode mode : BeanConflictMode.values()) {
			for (final String name : mode.getRecognizedNames()) {
				// Given
				// Add spaces on both sides and convert to uppercase
				final Reader xml = xml("<bean name=\"x\" conflict=\" " + name.toUpperCase() + " \" />");
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
			final Reader xml = xml("<propertyBundle name=\"x\" useResource=\"" + mode.getCanonicalName() + "\" />");
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
				final Reader xml = xml("<propertyBundle name=\"x\" useResource=\"" + name + "\" />");
				// When
				final Object parsedObj;
				try {
					parsedObj = xmlParser.unmarshal(xml);
					fail("Parsed despite invalid name " + name + " for mode " + mode);
				} catch (DescriptorParsingException e) {
					// ok
				}
			}
		}
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseBundleSourceModeAliasesInLenientMode() throws Exception {
		System.setProperty("auderis.deploy.lenient", Boolean.TRUE.toString());
		for (ExternalBundleSourceMode mode : ExternalBundleSourceMode.values()) {
			for (final String name : mode.getRecognizedNames()) {
				// Given
				// Add spaces on both sides and convert to uppercase
				final Reader xml = xml("<propertyBundle name=\"x\" useResource=\" " + name.toUpperCase() + " \" />");
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
