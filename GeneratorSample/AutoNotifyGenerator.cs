using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace GeneratorSample
{
    [Generator]
    public class AutoNotifyGenerator : ISourceGenerator
    {
        const string attributeAndHelperText = @"
using System;
namespace AutoNotify
{
    [AttributeUsage(AttributeTargets.Field, Inherited = false, AllowMultiple = false)]
    sealed class AutoNotifyAttribute : Attribute
    {
        public AutoNotifyAttribute()
        {
        }
        public string PropertyName { get; set; }
    }

    static class NotifyHelper
    {
        static public bool Equals<T>(T field, T value) => 
            System.Collections.Generic.EqualityComparer<T>.Default.Equals(field, value);
    }
}
";

        public void Initialize(GeneratorInitializationContext context)
        {
            // регистрируем syntax receiver, который будет создаваться на каждый пробег генерации
            context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
        }

        public void Execute(GeneratorExecutionContext context)
        {
            // Добавить текст атрибута и вспомогательный класс
            context.AddSource("AutoNotifyAttribute", SourceText.From(attributeAndHelperText, Encoding.UTF8));

            // получаем наш заполненный SyntaxReceiver
            if (!(context.SyntaxReceiver is SyntaxReceiver receiver))
                return;

            // мы создадим новую Compilation с атрибутом
            // TODO: в будущем будет разрешено добавлять исходники во время инициализации, так что этот шаг не будет нужен
            CSharpParseOptions options = (context.Compilation as CSharpCompilation).SyntaxTrees[0].Options as CSharpParseOptions;
            Compilation compilation = context.Compilation.AddSyntaxTrees(
                CSharpSyntaxTree.ParseText(SourceText.From(attributeAndHelperText, Encoding.UTF8), options));

            // получаем добавленный атрибут and INotifyPropertyChanged
            INamedTypeSymbol attributeSymbol = compilation.GetTypeByMetadataName("AutoNotify.AutoNotifyAttribute");
            INamedTypeSymbol notifySymbol = compilation.GetTypeByMetadataName("System.ComponentModel.INotifyPropertyChanged");

            // обходим поля-кандидаты, выбираем только те, у которых действительно есть нужный атрибут
            List<IFieldSymbol> fieldSymbols = new List<IFieldSymbol>();
            foreach (FieldDeclarationSyntax field in receiver.CandidateFields)
            {
                SemanticModel model = compilation.GetSemanticModel(field.SyntaxTree);
                foreach (VariableDeclaratorSyntax variable in field.Declaration.Variables)
                {
                    // получаем символ, декларируемый этим полем, и запоминаем его, если он и правда имеет атрибут
                    IFieldSymbol fieldSymbol = model.GetDeclaredSymbol(variable) as IFieldSymbol;
                    if (fieldSymbol.GetAttributes().Any(ad => ad.AttributeClass.Equals(attributeSymbol, SymbolEqualityComparer.Default)))
                        fieldSymbols.Add(fieldSymbol);
                }
            }

            // сгруппируем поля по классам, и сгенерируем новые исходные файлы
            foreach (IGrouping<INamedTypeSymbol, IFieldSymbol> group in fieldSymbols.GroupBy(f => f.ContainingType))
            {
                CompilationUnitSyntax classDef = ProcessClass(group.Key, group.ToList(), attributeSymbol, notifySymbol, context, options);
                if (classDef is null)
                    continue;
                var classSource = classDef.ToFullString();
                var filenameHint = $"{group.Key.Name}_autoNotify.cs";
                context.AddSource(filenameHint, SourceText.From(classSource, Encoding.UTF8));
            }
        }

        CompilationUnitSyntax ProcessClass(INamedTypeSymbol classSymbol, List<IFieldSymbol> fields, ISymbol attributeSymbol, ISymbol notifySymbol,
            GeneratorExecutionContext context, CSharpParseOptions options)
        {
            // проверим, а не вложенный ли это класс?
            if (!classSymbol.ContainingSymbol.Equals(classSymbol.ContainingNamespace, SymbolEqualityComparer.Default))
            {
                // TODO: выдать дагностическое сообщение, что класс должен быть на верхнем уровне
                return null;
            }

            string namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
            // будем класть наш partial-класс в то же пространство имён
            var ns = NamespaceDeclaration(ParseName(namespaceName))
                .AddUsings(UsingDirective(ParseName("System")),
                           UsingDirective(ParseName("System.ComponentModel")))
                .NormalizeWhitespace();

            // добавили использование System и System.ComponentModel
            // теперь определим класс и не забудем добавить partial

            var cl = ClassDeclaration(classSymbol.Name).AddModifiers(Token(SyntaxKind.PartialKeyword));

            // если класс не реализует INotifyPropertyChanged сам, добавим реализацию
            if (!classSymbol.Interfaces.Contains(notifySymbol))
            {
                cl = cl.AddBaseListTypes(SimpleBaseType(ParseTypeName(notifySymbol.ToDisplayString())))
                       .AddMembers(
                           EventDeclaration(ParseTypeName("System.ComponentModel.PropertyChangedEventHandler"), "PropertyChanged")
                            .AddModifiers(Token(SyntaxKind.PublicKeyword))
                            .WithSemicolonToken(Token(SyntaxKind.SemicolonToken))
                            .NormalizeWhitespace()
                       );
            }

            // для каждого из полей создадим свойство
            foreach (IFieldSymbol fieldSymbol in fields)
                cl = ProcessField(cl, fieldSymbol, attributeSymbol, context, options);

            // добавим декларацию класса в пространство имён
            ns = ns.AddMembers(cl);

            var unit = CompilationUnit().AddMembers(ns).NormalizeWhitespace();

            return unit;
        }

        static DiagnosticDescriptor FieldErrorDiagnostic =
            new DiagnosticDescriptor(
                "AutoNotifyGenerator",
                "Cannot generate property for this field",
                "Property cannot be generated for field {0}",
                "Code generation",
                DiagnosticSeverity.Error,
                isEnabledByDefault: true);
        private ClassDeclarationSyntax ProcessField(ClassDeclarationSyntax cl, IFieldSymbol fieldSymbol, ISymbol attributeSymbol,
            GeneratorExecutionContext context, CSharpParseOptions options)
        {
            // получим имя и тип поля
            string fieldName = fieldSymbol.Name;
            ITypeSymbol fieldType = fieldSymbol.Type;

            // получим атрибут AutoNotify у поля, и посмотрим, не указал ли программист конкретное имя для свойства
            AttributeData attributeData = fieldSymbol.GetAttributes().Single(ad => ad.AttributeClass.Equals(attributeSymbol, SymbolEqualityComparer.Default));
            TypedConstant overridenNameOpt = attributeData.NamedArguments.SingleOrDefault(kvp => kvp.Key == "PropertyName").Value;

            string propertyName = chooseName(fieldName, overridenNameOpt);
            if (propertyName.Length == 0 || propertyName == fieldName)
            {
                // не смогли создать имя для свойства? сообщим ошибку
                context.ReportDiagnostic(Diagnostic.Create(FieldErrorDiagnostic, fieldSymbol.Locations.FirstOrDefault(), fieldName));
                return cl;
            }

            // шаблон для декларации
            var declarationText = $@"
public {fieldType.ToDisplayString()} {propertyName} 
{{
    get => this.{fieldName};
    set
    {{
        if (AutoNotify.NotifyHelper.Equals(this.{fieldName}, value))
            return;
        {fieldName} = value;
        PropertyChanged?.Invoke(this, new System.ComponentModel.PropertyChangedEventArgs(nameof({propertyName})));
    }}
}}
";
            var subtree = ParseMemberDeclaration(declarationText, 0, options);
            cl = cl.AddMembers(subtree);

            return cl;

            string chooseName(string fieldName, TypedConstant overridenNameOpt)
            {
                if (!overridenNameOpt.IsNull)
                    return overridenNameOpt.Value.ToString();

                fieldName = fieldName.TrimStart('_');
                if (fieldName.Length == 0)
                    return string.Empty;

                if (fieldName.Length == 1)
                    return fieldName.ToUpper();

                return fieldName.Substring(0, 1).ToUpper() + fieldName.Substring(1);
            }
        }

        /// <summary>
        /// Создаётся на каждый пробег анализатора
        /// </summary>
        class SyntaxReceiver : ISyntaxReceiver
        {
            public List<FieldDeclarationSyntax> CandidateFields { get; } = new();

            /// <summary>
            /// Вызывается для каждого синтаксического узла в compilation, мы можем
            /// проверить узлы и сохранить информацию, полезную для генерации
            /// </summary>
            public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
            {
                // любое поле с атрибутом -- кандидат на рассмотрение
                if (syntaxNode is FieldDeclarationSyntax fieldDeclarationSyntax &&
                    fieldDeclarationSyntax.AttributeLists.Count > 0)
                {
                    CandidateFields.Add(fieldDeclarationSyntax);
                }
            }
        }
    }
}
