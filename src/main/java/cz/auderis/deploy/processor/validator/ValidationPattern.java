package cz.auderis.deploy.processor.validator;

import java.util.regex.Pattern;

public interface ValidationPattern {

	Pattern BEAN_NAME = Pattern.compile("[\\p{L}\\p{N}._:-]+");

	Pattern PROPERTY_NAME = Pattern.compile("[\\p{L}_$][\\p{L}\\p{N}_$]*");

	Pattern JAVA_CLASS = Pattern.compile("([\\p{L}_$][\\p{L}\\p{N}_$]*\\.)*[\\p{L}_$][\\p{L}\\p{N}_$]*");


}
