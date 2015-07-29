package cz.auderis.deploy.descriptor.producer;

import cz.auderis.deploy.descriptor.DeploymentEntry;
import cz.auderis.deploy.descriptor.DescriptorParsingException;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isBlank;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isStrictParsingEnabled;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.parseEnumByName;

@XmlRootElement(name = "factory")
@XmlType
public class FactoryDefinition extends DeploymentEntry {
	private static final long serialVersionUID = -6863617493100045759L;

	@XmlID
	@XmlAttribute(name = "name", required = true)
	protected String name;

	@XmlAttribute(name = "class", required = true)
	protected String factoryClass;

	@XmlAttribute(name = "method", required = true)
	protected String factoryMethod;

	@XmlTransient
	protected FactoryProductionMode productionMode;




	@XmlAttribute(name = "mode")
	protected final String getProductionModeCode() {
		return productionMode.getCanonicalName();
	}

	protected final void setProductionModeCode(String prodModeCode) {
		final FactoryProductionMode mode = parseEnumByName(prodModeCode, FactoryProductionMode.class);
		if (null != mode) {
			this.productionMode = mode;
		} else if (isBlank(prodModeCode) && !isStrictParsingEnabled()) {
			this.productionMode = FactoryProductionMode.getDefaultMode();
		} else {
			throw new DescriptorParsingException("Invalid factory production mode '" + prodModeCode + "'");
		}
	}

}
