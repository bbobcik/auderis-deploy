package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.DescriptorParserSupport;

import java.util.Set;

public enum BeanInstantiationMode implements DescriptorParserSupport.NamedEnum {

	ON_DEMAND("OnDemand", "Auto", "Optional", "On Demand", "on-demand", "Lazy"),

	ALWAYS("Always", "Required", "Mandatory", "Eager"),

	DISABLED("Disabled", "Never", "Forbidden")

	;

	private static BeanInstantiationMode DEFAULT_MODE = ON_DEMAND;
	private final String canonicalName;
	private final Set<String> recognizedNames;

	BeanInstantiationMode(String canonicalName, String... otherNames) {
		this.canonicalName = canonicalName;
		this.recognizedNames = DescriptorParserSupport.recognizedNameSet(canonicalName, otherNames);
	}

	public static BeanInstantiationMode getDefaultMode() {
		return DEFAULT_MODE;
	}

	public static void setDefaultMode(BeanInstantiationMode newDefaultMode) {
		if (null == newDefaultMode) {
			throw new NullPointerException();
		}
		DEFAULT_MODE = newDefaultMode;
	}

	@Override
	public String getCanonicalName() {
		return canonicalName;
	}

	@Override
	public Set<String> getRecognizedNames() {
		return recognizedNames;
	}

	@Override
	public String toString() {
		return canonicalName;
	}

}
