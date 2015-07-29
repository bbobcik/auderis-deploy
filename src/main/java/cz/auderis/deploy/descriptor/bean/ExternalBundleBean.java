package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.DescriptorParsingException;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import java.util.ResourceBundle;

import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isBlank;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isStrictParsingEnabled;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.parseEnumByName;

@XmlRootElement(name = "propertyBundle")
@XmlType()
public class ExternalBundleBean extends AbstractBean {
	private static final long serialVersionUID = 20150728L;

	@XmlTransient
	protected ExternalBundleSourceMode sourceMode;

	public ExternalBundleBean() {
		super();
		this.sourceMode = ExternalBundleSourceMode.getDefaultMode();
	}

	@Override
	public BeanType getBeanType() {
		return BeanType.EXTERNAL_BUNDLE;
	}

	@Override
	public String getBeanClassName() {
		return ResourceBundle.class.getName();
	}

	public ExternalBundleSourceMode getSourceMode() {
		return sourceMode;
	}

	@XmlAttribute(name = "combine")
	protected final String getSourceModeCode() {
		return sourceMode.getCanonicalName();
	}

	protected final void setSourceModeCode(String sourceModeCode) {
		final ExternalBundleSourceMode mode = parseEnumByName(sourceModeCode, ExternalBundleSourceMode.class);
		if (null != mode) {
			this.sourceMode = mode;
		} else if (isBlank(sourceModeCode) && !isStrictParsingEnabled()) {
			this.sourceMode = ExternalBundleSourceMode.getDefaultMode();
		} else {
			throw new DescriptorParsingException("Invalid bundle source mode '" + sourceModeCode + "'");
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		} else if (!super.equals(obj)) {
			return false;
		}
		assert obj instanceof ExternalBundleBean;
		final ExternalBundleBean other = (ExternalBundleBean) obj;
		if (sourceMode != other.getSourceMode()) {
			return false;
		}
		return true;
	}

}
