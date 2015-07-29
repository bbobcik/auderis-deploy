package cz.auderis.deploy.descriptor.initializer;

import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.dependency.PropertyInjectionElement;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlMixed;
import java.io.Serializable;
import java.util.List;

public abstract class AbstractInitializerContentHolder implements Serializable {
	private static final long serialVersionUID = 20150728L;

	@XmlMixed
	@XmlElementRefs({
			@XmlElementRef(type = BeanInjectionElement.class),
			@XmlElementRef(type = PropertyInjectionElement.class)
	})
	protected List<Object> contents;

}
