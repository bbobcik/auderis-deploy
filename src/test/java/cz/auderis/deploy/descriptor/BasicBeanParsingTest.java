package cz.auderis.deploy.descriptor;

import cz.auderis.deploy.descriptor.bean.BeanInstantiationMode;
import cz.auderis.deploy.descriptor.bean.BeanType;
import cz.auderis.deploy.descriptor.bean.ExternalBundleBean;
import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.deploy.descriptor.bean.StandaloneListBean;
import cz.auderis.deploy.descriptor.bean.StandaloneMapBean;
import cz.auderis.deploy.descriptor.bean.StandaloneSetBean;
import cz.auderis.test.category.SanityTest;
import cz.auderis.test.category.UnitTest;
import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.categories.Category;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import java.io.Reader;

import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

public class BasicBeanParsingTest {

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		final String packages = DescriptorParserSupport.getJaxbContextPackages();
		final JAXBContext jaxbCtx = JAXBContext.newInstance(packages);
		xmlParser = jaxbCtx.createUnmarshaller();
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseEmptyNormalBean() throws Exception {
		// Given
		final Reader xml = xml("<bean name=\"xyz\" class=\"java.lang.Object\" ></bean>");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(NormalBean.class));
		final NormalBean bean = (NormalBean) parsedObj;
		assertThat(bean, hasProperty("name", is("xyz")));
		assertThat(bean, hasProperty("beanType", is(BeanType.NORMAL)));
		assertThat(bean, hasProperty("beanClassName", is("java.lang.Object")));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseEmptyListBean() throws Exception {
		// Given
		final Reader xml = xml("<list name=\"list123\"></list>");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(StandaloneListBean.class));
		final StandaloneListBean bean = (StandaloneListBean) parsedObj;
		assertThat(bean, hasProperty("name", is("list123")));
		assertThat(bean, hasProperty("beanType", is(BeanType.LIST)));
		assertThat(bean, hasProperty("beanClassName", is("java.util.List")));
		assertThat(bean, hasProperty("itemClassName", is("java.lang.Object")));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseEmptySetBean() throws Exception {
		// Given
		final Reader xml = xml("<set name=\"setABC\"></set>");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(StandaloneSetBean.class));
		final StandaloneSetBean bean = (StandaloneSetBean) parsedObj;
		assertThat(bean, hasProperty("name", is("setABC")));
		assertThat(bean, hasProperty("beanType", is(BeanType.SET)));
		assertThat(bean, hasProperty("beanClassName", is("java.util.Set")));
		assertThat(bean, hasProperty("itemClassName", is("java.lang.Object")));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseEmptyMapBean() throws Exception {
		// Given
		final Reader xml = xml("<map name=\"aMap\"></map>");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(StandaloneMapBean.class));
		final StandaloneMapBean bean = (StandaloneMapBean) parsedObj;
		assertThat(bean, hasProperty("name", is("aMap")));
		assertThat(bean, hasProperty("beanType", is(BeanType.MAP)));
		assertThat(bean, hasProperty("beanClassName", is("java.util.Map")));
		assertThat(bean, hasProperty("keyClassName", is("java.lang.String")));
		assertThat(bean, hasProperty("valueClassName", is("java.lang.Object")));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseEmptyBundleBean() throws Exception {
		// Given
		final Reader xml = xml("<propertyBundle name=\"bundle\"></propertyBundle>");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(ExternalBundleBean.class));
		final ExternalBundleBean bean = (ExternalBundleBean) parsedObj;
		assertThat(bean, hasProperty("name", is("bundle")));
		assertThat(bean, hasProperty("beanType", is(BeanType.EXTERNAL_BUNDLE)));
		assertThat(bean, hasProperty("beanClassName", is("java.util.ResourceBundle")));
	}




}
