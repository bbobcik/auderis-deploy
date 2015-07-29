package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.DescriptorParserSupport;

import java.util.Set;

public enum BeanConflictMode implements DescriptorParserSupport.NamedEnum {

	FAIL("Fail", "Error", "Strict", "Forbidden"),

	LAZY_FAIL("FailOnUse", "Fail on use", "fail-on-use"),

	IGNORE("Ignore", "Skip", "KeepFirst", "Keep first", "keep-first"),

	REPLACE("Replace", "KeepLast", "Keep last", "keep-last"),

	UPDATE("Update", "Override", "Incremental"),

	;

	private static BeanConflictMode DEFAULT_MODE = FAIL;
	private final String canonicalName;
	private final Set<String> recognizedNames;

	BeanConflictMode(String canonicalName, String... otherNames) {
		this.canonicalName = canonicalName;
		this.recognizedNames = DescriptorParserSupport.recognizedNameSet(canonicalName, otherNames);
	}

	public static BeanConflictMode getDefaultMode() {
		return DEFAULT_MODE;
	}

	public static void setDefaultMode(BeanConflictMode newDefaultMode) {
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
