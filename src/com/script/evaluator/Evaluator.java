package com.script.evaluator;

import com.context.index.Index;
import com.script.evaluator.function.other.EfImport;
import com.json.JObject;
import com.script.evaluator.element.*;
import com.script.evaluator.function.*;
import com.script.evaluator.function.math.EfAbs;
import com.script.evaluator.function.other.*;
import com.script.evaluator.function.string.*;

import com.script.evaluator.operator.*;
import com.script.evaluator.operator.AndOperator.And1Operator;
import com.script.evaluator.operator.AndOperator.And2Operator;
import com.script.evaluator.operator.AssignOperator.Assign1Operator;
import com.script.evaluator.operator.AssignOperator.Assign2Operator;
import com.script.evaluator.operator.NotEqualOperator.NotEqual1Operator;
import com.script.evaluator.operator.NotEqualOperator.NotEqual2Operator;
import com.script.evaluator.operator.OrOperator.Or1Operator;
import com.script.evaluator.operator.OrOperator.Or2Operator;
import com.utils.reflections.TClass;
import java.util.*;

// ToDo: a = 1 + [1] wywołuje StackOverflow
public class Evaluator extends ECollection {

    boolean singleQuota = true;
    boolean sections = true;
    public final LinkedList<Element> subscripts = new LinkedList<>();

    public final LinkedHashMap<String, Operator> operators = new LinkedHashMap<>();

    public final Map<String, Function> functions = new LinkedHashMap<>();
    public final Map<String, Object> variables = new LinkedHashMap<>();
    public final Map<String, Object> constants = new LinkedHashMap<>();
    public final Set<String> keywords = new HashSet<>();

    private VariableResolver variableResolver = null;
    private VariableAssigner variableAssigner = null;
    private FunctionInvoker functionInvoker = null;

    String expression;
    String file;

    public JObject vars = new JObject();

    public Evaluator(String script) {
        this(null, script);
    }

    public Evaluator(String file, String script) {
        super(null, new Pos(file, 1, 1));
        constants.put("null", null);
        constants.put("true", true);
        constants.put("false", false);
        // constants.put("undefined", Undefined.TYPE);

        keywords.add("new");

        this.expression = script;
        this.file = file;

        new AdditionOperator(this);
        new SubtractionOperator(this);
        new MultiplicationOperator(this);
        new DivisionOperator(this);
        new EqualOperator(this);
        new NotEqual1Operator(this);
        new NotEqual2Operator(this);

        new LessThanOrEqualOperator(this);
        new LessThanOperator(this);
        new GreaterThanOrEqualOperator(this);
        new GreaterThanOperator(this);

        new Assign1Operator(this);
        new Assign2Operator(this);
        new And1Operator(this);
        new And2Operator(this);
        new Or1Operator(this);
        new Or2Operator(this);
        new NotOperator(this);
        new ModuloOperator(this);

        //--------------------------------------------------------
        addFunction(new EfThrow(this));
        addFunction(new EfImport(this));
        addFunction(new EfEval(this));
        addFunction(new EfRead(this));
        addFunction(new EfPush(this));
        addFunction(new EfConsole(this));
        addFunction(new EfLog(this));

        addFunction(new EfEnv(this));
        addFunction(new EfProp(this));

        addFunction(new EfAbs(this));/*
        addFunction(new Acos());
        addFunction(new Asin());
        addFunction(new Atan());
        addFunction(new Atan2());
        addFunction(new Ceil());
        addFunction(new Cos());
        addFunction(new Exp());
        addFunction(new Floor());
        addFunction(new IEEEremainder());
        addFunction(new EfLog());
        addFunction(new Max());
        addFunction(new Min());
        addFunction(new Pow());
        addFunction(new Random());
        addFunction(new Rint());
        addFunction(new Round());
        addFunction(new Sin());
        addFunction(new Sqrt());
        addFunction(new Tan());
        addFunction(new ToDegrees());
        addFunction(new ToRadians());
         */
        //--------------------------------
        addFunction(new EfCharAt(this));
        addFunction(new EfLowerCase(this));
        addFunction(new EfUpperCase(this));
        addFunction(new EfStartsWith(this));
        addFunction(new EfEndsWith(this));
        addFunction(new EfContains(this));

        /*
        addFunction(new CompareTo());
        addFunction(new CompareToIgnoreCase());
        addFunction(new Concat());
        addFunction(new EndsWith());
        addFunction(new Equals());
        addFunction(new EqualsIgnoreCase());
        addFunction(new EfEval());
        addFunction(new IndexOf());
        addFunction(new LastIndexOf());
        addFunction(new Length());
        addFunction(new Replace());
        addFunction(new StartsWith());
        addFunction(new Substring());
        addFunction(new ToLowerCase());
        addFunction(new ToUpperCase());
        addFunction(new Trim());*/
    }

    @Override
    public EValue eval() {
        parse(file, expression);
        EValue result = null;
        for (Element val : children)
            result = val.evalOrDef(result);
        return result;
    }

    public Evaluator addFunction(Function function) {
        if (functions.containsKey(function.name))
            throw new EvaluationException("Function \""
                    + function.name + "\" already exists");
        functions.put(function.name, function);
        return this;
    }

    public Evaluator singleQuota(boolean singleQuota) {
        this.singleQuota = singleQuota;
        return this;
    }

    public VariableResolver getVariableResolver() {
        return variableResolver;
    }

    public Evaluator setVariableResolver(VariableResolver variableResolver) {
        this.variableResolver = variableResolver;
        return this;
    }

    public VariableAssigner getVariableAssigner() {
        return variableAssigner;
    }

    public Evaluator setVariableAssigner(VariableAssigner variableAssigner) {
        this.variableAssigner = variableAssigner;
        return this;
    }

    public Evaluator setFunctionInvoker(FunctionInvoker functionInvoker) {
        this.functionInvoker = functionInvoker;
        return this;
    }

    public FunctionInvoker getFunctionInvoker() {
        return functionInvoker;
    }

}
